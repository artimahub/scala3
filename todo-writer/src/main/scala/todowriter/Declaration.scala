package todowriter

import scala.util.matching.Regex

/** The kind of a Scala declaration. */
enum DeclKind:
  case Def, Class, Trait, Object, Val, Var, Unknown

/** Represents a parsed Scala declaration. */
case class Declaration(
    kind: DeclKind,
    name: String,
    tparams: List[String],
    params: List[String],
    returnType: Option[String]
)

object Declaration:
  /** Regex to detect the start of a declaration (with optional modifiers/annotations). */
  private val DeclStartPattern: Regex =
    """(?:@[\w\(\)\s,."]+\s*)*(?:private|protected|final|override|inline|implicit|given|export|opaque|sealed|abstract|lazy|case\s+)*\s*(class|trait|object|def|val|var)\b""".r

  /** Check whether the next nonblank chunk starts with a declaration. */
  def startsWithDeclaration(chunk: String): Boolean =
    DeclStartPattern.findPrefixMatchOf(chunk.trim).nonEmpty

  /** Check whether a line could be the start of a declaration prefix.
   *
   *  This accepts standalone annotations and modifier-only lines so that
   *  multi-line declarations can still be scanned, but rejects ordinary code
   *  expressions inside method bodies.
   */
  private def startsDeclarationPrefix(line: String): Boolean =
    val trimmed = line.trim
    startsWithDeclaration(trimmed) ||
    trimmed.startsWith("@") ||
    trimmed.matches(
      """(?:private(?:\[[^\]]+\])?|protected(?:\[[^\]]+\])?|final|override|inline|implicit|given|export|opaque|sealed|abstract|lazy|case)\b.*"""
    )

  /** Regex to parse a def declaration.
   *
   *  Supports one level of nested `[]` in type params (e.g. `F[_]`) and
   *  one level of nested `()` in parameter types (e.g. `(Int, Int) => Int`).
   */
  private val DefPattern: Regex =
    """def\s+([^\s\(\[:\=]+)\s*(?:\[((?:[^\[\]]|\[[^\[\]]*\])*)\])?\s*((?:\((?:[^()]|\([^()]*\))*\))*)\s*(?::\s*([^=\{]+))?""".r

  /** Regex to parse a class/trait declaration.
   *
   *  Uses the same nested-bracket / nested-parenthesis handling as defs,
   *  including multiple constructor parameter lists.
   */
  private val ClassPattern: Regex =
    """(class|trait)\s+([^\s\(\[]+)\s*(?:\[((?:[^\[\]]|\[[^\[\]]*\])*)\])?\s*((?:\((?:[^()]|\([^()]*\))*\))*)""".r

  /** Regex to parse an object declaration. */
  private val ObjectPattern: Regex =
    """object\s+([^\s\(\[{:]+)""".r

  /** Regex to parse a val/var declaration. */
  private val ValVarPattern: Regex =
    """(val|var)\s+([A-Za-z0-9_]+)""".r

  /** Parse a declaration from the text chunk following a Scaladoc block. */
  def parse(chunk: String): Declaration =
    val trimmed = chunk.trim

    // Detect declaration kind
    val kindMatch = DeclStartPattern.findFirstMatchIn(trimmed)
    val kind = kindMatch.map(_.group(1)) match
      case Some("def")    => DeclKind.Def
      case Some("class")  => DeclKind.Class
      case Some("trait")  => DeclKind.Trait
      case Some("object") => DeclKind.Object
      case Some("val")    => DeclKind.Val
      case Some("var")    => DeclKind.Var
      case _              => DeclKind.Unknown

    kind match
      case DeclKind.Def     => parseDef(trimmed)
      case DeclKind.Class   => parseClass(trimmed, DeclKind.Class)
      case DeclKind.Trait   => parseClass(trimmed, DeclKind.Trait)
      case DeclKind.Object  => parseObject(trimmed)
      case DeclKind.Val     => parseValVar(trimmed, DeclKind.Val)
      case DeclKind.Var     => parseValVar(trimmed, DeclKind.Var)
      case DeclKind.Unknown => Declaration(DeclKind.Unknown, "", Nil, Nil, None)

  private def parseDef(chunk: String): Declaration =
    // Normalize chunk: join lines, collapse whitespace
    val normalized = chunk.linesIterator.mkString(" ").replaceAll("\\s+", " ")
    val defIdx = normalized.indexOf("def ")
    if defIdx < 0 then Declaration(DeclKind.Def, "", Nil, Nil, None)
    else
      var i = defIdx + 4
      while i < normalized.length && normalized(i).isWhitespace do i += 1

      val nameStart = i
      while i < normalized.length &&
          (normalized(i).isLetterOrDigit || normalized(i) == '_' || normalized(i) == '$') do i += 1
      val name = normalized.substring(nameStart, i)

      while i < normalized.length && normalized(i).isWhitespace do i += 1

      val tparamsStr =
        if i < normalized.length && normalized(i) == '[' then
          val end = findBalancedEnd(normalized, i, '[', ']')
          if end > i then
            val inside = normalized.substring(i + 1, end)
            i = end + 1
            while i < normalized.length && normalized(i).isWhitespace do i += 1
            inside
          else ""
        else ""

      val paramsSb = new StringBuilder
      var keepReadingParams = true
      while keepReadingParams do
        while i < normalized.length && normalized(i).isWhitespace do i += 1
        if i < normalized.length && normalized(i) == '(' then
          val end = findBalancedEnd(normalized, i, '(', ')')
          if end > i then
            paramsSb.append(normalized.substring(i, end + 1))
            i = end + 1
          else keepReadingParams = false
        else keepReadingParams = false

      while i < normalized.length && normalized(i).isWhitespace do i += 1
      val returnTypeStr =
        if i < normalized.length && normalized(i) == ':' then
          Some(normalized.substring(i + 1).trim)
        else None

      val tparams = parseTypeParams(tparamsStr)
      val params = parseParams(paramsSb.toString)
      val returnType = returnTypeStr.map(cleanReturnType)

      Declaration(DeclKind.Def, name, tparams, params, returnType)

  private def parseClass(chunk: String, kind: DeclKind): Declaration =
    val normalized = chunk.linesIterator.mkString(" ").replaceAll("\\s+", " ")

    ClassPattern.findFirstMatchIn(normalized) match
      case Some(m) =>
        val name = m.group(2)
        val tparamsStr = Option(m.group(3)).getOrElse("")
        val paramsStr = Option(m.group(4)).getOrElse("")

        val tparams = parseTypeParams(tparamsStr)
        val params = parseParams(paramsStr)

        Declaration(kind, name, tparams, params, None)
      case None =>
        Declaration(kind, "", Nil, Nil, None)

  private def parseObject(chunk: String): Declaration =
    ObjectPattern.findFirstMatchIn(chunk) match
      case Some(m) =>
        Declaration(DeclKind.Object, m.group(1), Nil, Nil, None)
      case None =>
        Declaration(DeclKind.Object, "", Nil, Nil, None)

  private def parseValVar(chunk: String, kind: DeclKind): Declaration =
    ValVarPattern.findFirstMatchIn(chunk) match
      case Some(m) =>
        Declaration(kind, m.group(2), Nil, Nil, None)
      case None =>
        Declaration(kind, "", Nil, Nil, None)

  /** Parse type parameter names from a string like "A, B <: Foo, C". */
  private def parseTypeParams(str: String): List[String] =
    if str.trim.isEmpty then Nil
    else
      splitByCommasTopLevel(str).map { tp =>
        // Extract just the name, before any bounds or variance
        tp.trim
          .stripPrefix("+")
          .stripPrefix("-")
          .trim
          .takeWhile(c => c.isLetterOrDigit || c == '_')
      }.filter(_.nonEmpty)

  /** Parse parameter names from a string like "(x: Int, y: String)(z: Double)". */
  private def parseParams(str: String): List[String] =
    if str.trim.isEmpty then Nil
    else
      val groups = collectParamGroups(str)

      groups.flatMap { group =>
        splitByCommasTopLevel(group).flatMap { param =>
          extractParamName(param.trim)
        }
      }

  /** Collect top-level (...) parameter groups while tolerating nested parens in annotations/types. */
  private def collectParamGroups(str: String): List[String] =
    val groups = collection.mutable.ListBuffer[String]()
    var i = 0
    while i < str.length do
      if str(i) == '(' then
        val start = i + 1
        var depth = 1
        var inString = false
        var quoteChar = '\u0000'
        var escaped = false
        i += 1
        while i < str.length && depth > 0 do
          val ch = str(i)
          if inString then
            if escaped then escaped = false
            else if ch == '\\' then escaped = true
            else if ch == quoteChar then inString = false
          else
            if ch == '"' || ch == '\'' then
              inString = true
              quoteChar = ch
            else if ch == '(' then depth += 1
            else if ch == ')' then depth -= 1
          i += 1
        if depth == 0 then
          groups += str.substring(start, i - 1)
        else
          i = str.length
      else i += 1
    groups.toList

  /** Find the matching closing delimiter for a balanced section starting at `start`. */
  private def findBalancedEnd(str: String, start: Int, open: Char, close: Char): Int =
    if start < 0 || start >= str.length || str(start) != open then -1
    else
      var i = start
      var depth = 0
      var inString = false
      var quoteChar = '\u0000'
      var escaped = false
      while i < str.length do
        val ch = str(i)
        if inString then
          if escaped then escaped = false
          else if ch == '\\' then escaped = true
          else if ch == quoteChar then inString = false
        else
          if ch == '"' || ch == '\'' then
            inString = true
            quoteChar = ch
          else if ch == open then depth += 1
          else if ch == close then
            depth -= 1
            if depth == 0 then return i
        i += 1
      -1

  /** Extract parameter name from a param declaration like "x: Int" or "implicit ev: Eq[T]". */
  private def extractParamName(param: String): Option[String] =
    if param.isEmpty then None
    else
      var trimmed = param.trim
      trimmed = dropLeadingAnnotations(trimmed)
      // Handle implicit/using/given keywords
      if trimmed.startsWith("implicit ") then trimmed = trimmed.drop(9).trim
      else if trimmed.startsWith("using ") then trimmed = trimmed.drop(6).trim
      else if trimmed.startsWith("given ") then trimmed = trimmed.drop(6).trim

      // Handle val/var modifiers (for class constructor params)
      if trimmed.startsWith("val ") then trimmed = trimmed.drop(4).trim
      else if trimmed.startsWith("var ") then trimmed = trimmed.drop(4).trim
      trimmed = dropLeadingAnnotations(trimmed)

      // Handle override/private/protected modifiers
      if trimmed.startsWith("override ") then trimmed = trimmed.drop(9).trim
      if trimmed.startsWith("private ") then trimmed = trimmed.drop(8).trim
      else if trimmed.startsWith("protected ") then trimmed = trimmed.drop(10).trim

      // Check for val/var again after modifiers
      if trimmed.startsWith("val ") then trimmed = trimmed.drop(4).trim
      else if trimmed.startsWith("var ") then trimmed = trimmed.drop(4).trim
      trimmed = dropLeadingAnnotations(trimmed)

      // Unnamed contextual parameters like `(using Context)` have no identifier,
      // so they should not produce an @param tag.
      val colonIdx = trimmed.indexOf(':')
      if colonIdx < 0 then None
      else
        val name = trimmed.substring(0, colonIdx).trim

        if name.nonEmpty && (name.head.isLetter || name.head == '_') then Some(name) else None

  /** Remove leading parameter annotations, including annotation arguments. */
  private def dropLeadingAnnotations(str: String): String =
    var remaining = str.trim
    var changed = true
    while changed && remaining.startsWith("@") do
      val end = leadingAnnotationEnd(remaining)
      if end > 0 then
        remaining = remaining.substring(end).trim
      else
        changed = false
    remaining

  /** Find the end index (exclusive) of a leading annotation, or -1 if invalid. */
  private def leadingAnnotationEnd(str: String): Int =
    if str.isEmpty || str.head != '@' then -1
    else
      var i = 1
      while i < str.length && (str(i).isLetterOrDigit || str(i) == '_' || str(i) == '.') do i += 1
      while i < str.length && str(i).isWhitespace do i += 1

      if i < str.length && str(i) == '(' then
        var depth = 0
        var inString = false
        var quoteChar = '\u0000'
        var escaped = false

        while i < str.length do
          val ch = str(i)
          if inString then
            if escaped then escaped = false
            else if ch == '\\' then escaped = true
            else if ch == quoteChar then inString = false
          else
            if ch == '"' || ch == '\'' then
              inString = true
              quoteChar = ch
            else if ch == '(' then depth += 1
            else if ch == ')' then
              depth -= 1
              if depth == 0 then return i + 1
          i += 1
        -1
      else i

  /** Split a string by commas, but ignore commas inside brackets/parentheses. */
  private def splitByCommasTopLevel(str: String): List[String] =
    val parts = collection.mutable.ListBuffer[String]()
    val current = new StringBuilder
    var depth = 0

    for ch <- str do
      ch match
        case '(' | '[' | '{' =>
          depth += 1
          current += ch
        case ')' | ']' | '}' =>
          depth = math.max(0, depth - 1)
          current += ch
        case ',' if depth == 0 =>
          parts += current.toString
          current.clear()
        case _ =>
          current += ch

    val last = current.toString.trim
    if last.nonEmpty then parts += last
    parts.toList

  /** Clean up a return type string. */
  private def cleanReturnType(str: String): String =
    // Remove trailing = or { and anything after
    val cleaned = str.split("[=\\{]").head.trim
    // Remove trailing : if present
    if cleaned.endsWith(":") then cleaned.dropRight(1).trim else cleaned

  /** Get the declaration chunk following a Scaladoc block. */
  def getDeclarationAfter(text: String, scaladocEndIndex: Int): String =
    val tail = text.substring(scaladocEndIndex)
    val lines = tail.linesIterator.toList

    // Skip blank lines and collect until we see a brace, equals, or end of signature
    val chunkLines = collection.mutable.ListBuffer[String]()
    var done = false
    var lineCount = 0
    val maxLines = 40

    val firstNonBlank = lines.iterator.dropWhile(_.trim.isEmpty).take(1).toList.headOption.map(_.trim)
    if firstNonBlank.exists(line => line.startsWith("package ") || line.startsWith("import ")) then
      firstNonBlank.get
    else if firstNonBlank.forall(line => !startsDeclarationPrefix(line)) then
      firstNonBlank.getOrElse("")
    else

      for line <- lines if !done && lineCount < maxLines do
        lineCount += 1
        if lineCount == 1 && line.trim.isEmpty then
          // Skip leading blank line
          ()
        else
          chunkLines += line
          val trimmed = line.trim
          // Stop when we see opening brace, equals sign, or end of signature
          if trimmed.contains("{") || trimmed.contains("=") ||
             trimmed.endsWith(")") || trimmed.endsWith(":") then
            done = true

      chunkLines.mkString("\n")
