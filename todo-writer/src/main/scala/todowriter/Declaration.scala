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

  /** Regex to parse a def declaration. */
  private val DefPattern: Regex =
    """def\s+([^\s\(\[:\=]+)\s*(?:\[([^\]]*)\])?\s*((?:\([^)]*\))*)\s*(?::\s*([^=\{]+))?""".r

  /** Regex to parse a class/trait declaration. */
  private val ClassPattern: Regex =
    """(class|trait)\s+([^\s\(\[]+)\s*(?:\[([^\]]*)\])?\s*((?:\([^)]*\))?)""".r

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

    DefPattern.findFirstMatchIn(normalized) match
      case Some(m) =>
        val name = m.group(1)
        val tparamsStr = Option(m.group(2)).getOrElse("")
        val paramsStr = Option(m.group(3)).getOrElse("")
        val returnTypeStr = Option(m.group(4)).map(_.trim)

        val tparams = parseTypeParams(tparamsStr)
        val params = parseParams(paramsStr)
        val returnType = returnTypeStr.map(cleanReturnType)

        Declaration(DeclKind.Def, name, tparams, params, returnType)
      case None =>
        Declaration(DeclKind.Def, "", Nil, Nil, None)

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
      // Find all parenthesis groups
      val groupPattern = """\(([^)]*)\)""".r
      val groups = groupPattern.findAllMatchIn(str).map(_.group(1)).toList

      groups.flatMap { group =>
        splitByCommasTopLevel(group).flatMap { param =>
          extractParamName(param.trim)
        }
      }

  /** Extract parameter name from a param declaration like "x: Int" or "implicit ev: Eq[T]". */
  private def extractParamName(param: String): Option[String] =
    if param.isEmpty then None
    else
      var trimmed = param.trim
      // Handle implicit/using/given keywords
      if trimmed.startsWith("implicit ") then trimmed = trimmed.drop(9).trim
      else if trimmed.startsWith("using ") then trimmed = trimmed.drop(6).trim
      else if trimmed.startsWith("given ") then trimmed = trimmed.drop(6).trim

      // Handle val/var modifiers (for class constructor params)
      if trimmed.startsWith("val ") then trimmed = trimmed.drop(4).trim
      else if trimmed.startsWith("var ") then trimmed = trimmed.drop(4).trim

      // Handle override/private/protected modifiers
      if trimmed.startsWith("override ") then trimmed = trimmed.drop(9).trim
      if trimmed.startsWith("private ") then trimmed = trimmed.drop(8).trim
      else if trimmed.startsWith("protected ") then trimmed = trimmed.drop(10).trim

      // Check for val/var again after modifiers
      if trimmed.startsWith("val ") then trimmed = trimmed.drop(4).trim
      else if trimmed.startsWith("var ") then trimmed = trimmed.drop(4).trim

      // Extract name before ':'
      val colonIdx = trimmed.indexOf(':')
      val name =
        if colonIdx > 0 then trimmed.substring(0, colonIdx).trim
        else trimmed.takeWhile(c => c.isLetterOrDigit || c == '_')

      if name.nonEmpty && name.head.isLetter then Some(name) else None

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
