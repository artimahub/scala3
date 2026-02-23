package todowriter

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import java.net.{HttpURLConnection, URL}

/** Result of checking a single Scaladoc/declaration pair. */
case class CheckResult(
    scaladoc: ScaladocBlock,
    declaration: Declaration,
    issues: List[Issue]
)

/** Result of checking a single file. */
case class FileResult(
    path: String,
    results: List[CheckResult]
):
  def hasIssues: Boolean = results.exists(_.issues.nonEmpty)
  def issueCount: Int = results.map(_.issues.size).sum

/** Summary statistics for a check run. */
case class Summary(
    totalFiles: Int,
    totalIssues: Int,
    defTotal: Int,
    defWithIssues: Int,
    classTotal: Int,
    classWithIssues: Int,
    traitTotal: Int,
    traitWithIssues: Int
)

object ScaladocChecker:
  /** Find all .scala files recursively under the given directory. */
  def findScalaFiles(root: Path): List[Path] =
    if !Files.isDirectory(root) then Nil
    else
      Files
        .walk(root)
        .iterator()
        .asScala
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".scala"))
        .toList

  /** Check a single file and return results. */
  def checkFile(path: Path): FileResult =
    val text = Files.readString(path)
    val blocks = ScaladocBlock.findAll(text)

    val results = blocks.map { block =>
      val chunk = Declaration.getDeclarationAfter(text, block.endIndex)
      val decl = Declaration.parse(chunk)
      val issues = validate(block, decl)
      CheckResult(block, decl, issues)
    }

    FileResult(path.toString, results)

  /** Validate a Scaladoc block against its declaration. */
  def validate(block: ScaladocBlock, decl: Declaration): List[Issue] =
    val issues = collection.mutable.ListBuffer[Issue]()

    // Only validate @param and @tparam for def, class, trait
    if decl.kind == DeclKind.Def || decl.kind == DeclKind.Class || decl.kind == DeclKind.Trait then
      // Check @param
      val missingParams = decl.params.filterNot(block.params.contains)
      val unknownParams = block.params.filterNot(decl.params.contains)

      if missingParams.nonEmpty then
        issues += Issue.MissingParam(missingParams)
      if unknownParams.nonEmpty then
        issues += Issue.UnknownParam(unknownParams)

      // Check @tparam
      val missingTparams = decl.tparams.filterNot(block.tparams.contains)
      val unknownTparams = block.tparams.filterNot(decl.tparams.contains)

      if missingTparams.nonEmpty then
        issues += Issue.MissingTparam(missingTparams)
      if unknownTparams.nonEmpty then
        issues += Issue.UnknownTparam(unknownTparams)

    // Only validate @return for def
    if decl.kind == DeclKind.Def then
      decl.returnType match
        case Some(ret) if ret.trim.startsWith("Unit") =>
          // Unit return type - should NOT have @return
          if block.hasReturn then
            issues += Issue.UnnecessaryReturn
        case Some(ret) =>
          // Non-Unit return type - should have @return (unless one-liner)
          if !block.hasReturn && !block.isOneLiner then
            issues += Issue.MissingReturn
        case None =>
          // Unknown return type - skip validation
          ()

    issues.toList

  /** Check all files under a directory and return results. */
  def checkDirectory(root: Path): List[FileResult] =
    findScalaFiles(root).map(checkFile)
  
  /** Return true if the given fully-qualified or simple symbol exists among
    *  .scala source files under `root`. This is public for testing.
    */
  def symbolExistsInSource(root: Path, symbol: String): Boolean =
    // symbol like "com.example.Foo" -> pkg="com.example", name="Foo"
    val parts = symbol.split('.')
    if parts.isEmpty then false
    else
      val name = parts.last
      val files = findScalaFiles(root)
      val declPattern = raw"""(?m)^\s*(?:class|object|trait|type|case\s+class)\s+%s\b""".format(java.util.regex.Pattern.quote(name)).r
      val pkgPattern = raw"""(?m)^\s*package\s+([^\s;]+)""".r
      files.exists { p =>
        try
          val txt = Files.readString(p)
          // If package declaration exists, check full name match first
          val pkgOpt = pkgPattern.findFirstMatchIn(txt).map(_.group(1))
          val fullNameMatches =
            pkgOpt match
              case Some(pkg) => pkg + "." + name == symbol && declPattern.findFirstIn(txt).isDefined
              case None      => declPattern.findFirstIn(txt).isDefined && name == symbol
          if fullNameMatches then true
          else
            // Also accept top-level declaration without package when symbol is unqualified
            if !symbol.contains('.') && declPattern.findFirstIn(txt).isDefined then true
            else false
        catch
          case _: Throwable => false
      }
  /** Return true if the given package exists among .scala source files under `root`. */
  def packageExistsInSource(root: Path, pkg: String): Boolean =
    val files = findScalaFiles(root)
    // Check for various ways packages can be declared:
    // 1. package scala.math (single declaration)
    // 2. package scala followed by package math (nested declarations)
    // 3. package scala followed by package object math (package object)
    val parts = pkg.split('.')
    files.exists { p =>
      try
        val txt = Files.readString(p)
        // Extract all package declarations from the file
        val pkgPattern = raw"""(?m)^\s*package\s+([^\s;]+)""".r
        val packageObjectPattern = raw"""(?m)^\s*package\s+object\s+(\w+)""".r
        val pkgs = pkgPattern.findAllMatchIn(txt).map(_.group(1)).toList
        val packageObjects = packageObjectPattern.findAllMatchIn(txt).map(_.group(1)).toList

        // Check if the full package is declared
        if pkgs.contains(pkg) then true
        else
          // Check for nested packages (e.g., package scala followed by package math)
          val nestedPkg = parts.mkString(" ")
          if pkgs.contains(nestedPkg) then true
          else
            // Check for package object (e.g., package scala followed by package object math)
            val parent = parts.init.mkString(".")
            val child = parts.last
            pkgs.contains(parent) && packageObjects.contains(child)
      catch
        case _: Throwable => false
    }

  /** Find broken HTTP/HTTPS links referenced inside Scaladoc blocks using a custom checker.
    *
    *  Returns a list of tuples: (filePath, lineNumber, url, errorDescription)
    *  The lineNumber points to the Scaladoc physical line containing the link.
    *
    *  This variant is useful for testing (inject a fake checker). The checker
    *  should return None for a healthy URL, or Some(errorDescription) for a broken URL.
    */
  def findBrokenLinks(root: Path, checkUrl: String => Option[String]): List[(String, Int, String, String)] =
    val urlRegex = raw"https?://[^\s\)\]\}\>\"']+".r
    val files = findScalaFiles(root)
    val results = collection.mutable.ListBuffer[(String, Int, String, String)]()

    // Mask code regions so link-like patterns inside code are ignored while preserving
    // original string length and newline positions (so line/offset calculations remain valid).
    def maskRegionPreserveNewlines(s: String): String =
      s.map { ch => if ch == '\n' then '\n' else ' ' }.mkString

    def maskCodeRegions(s: String): String =
      var res = s
      val fenced = raw"(?s)```.*?```".r
      res = fenced.replaceAllIn(res, m => maskRegionPreserveNewlines(m.matched))
      val triple = raw"(?s)\{\{\{.*?\}\}\}".r
      res = triple.replaceAllIn(res, m => maskRegionPreserveNewlines(m.matched))
      val inline = raw"`[^`]*`".r
      res = inline.replaceAllIn(res, m => maskRegionPreserveNewlines(m.matched))
      res

    for path <- files do
      val text = Files.readString(path)
      val blocks = ScaladocBlock.findAll(text)
      for block <- blocks do
        // Use masked content to avoid detecting links that appear inside code spans
        var masked = maskCodeRegions(block.content)

        // Also mask wikidoc links to prevent markdown link extractor from matching inner patterns
        val wikidocMasked = raw"(?s)\[\[.*?\]\]".r.replaceAllIn(masked, m => maskRegionPreserveNewlines(m.matched))

        // Collect link matches from markdown [label](link), wikidoc [[...]], and raw http(s) URLs,
        // preserving the match start index so we can compute the physical line.
        val matches = collection.mutable.ListBuffer[(String, Int)]()

        // Extract wikidoc links [[...]] and parse them from the original block content
        def extractWikiLinks(s: String): List[(String, Int)] =
          val out = collection.mutable.ListBuffer[(String, Int)]()
          var i = 0
          while i < s.length do
            val start = s.indexOf("[[", i)
            if start < 0 then i = s.length
            else
              val end = s.indexOf("]]", start + 2)
              if end < 0 then i = start + 2
              else
                val content = s.substring(start + 2, end)
                // Wikidoc link format: [[symbol display text]] or [[https://url display text]]
                // The symbol/URL is the part before the first space
                val spaceIdx = content.indexOf(' ')
                var symbolOrUrl = if spaceIdx > 0 then content.substring(0, spaceIdx) else content
                // Unescape backslash-dot sequences (e.g., scala\.collection\.mutable -> scala.collection.mutable)
                symbolOrUrl = symbolOrUrl.replace("\\.", ".")
                out += ((symbolOrUrl, start))
                i = end + 2
          out.toList

        // Extract markdown links robustly (handle nested parentheses inside link target).
        // Returns (target, startIndex, malformedFlag) where malformedFlag=true when the
        // opening '(' has no matching closing ')'.
        def extractMdLinks(s: String): List[(String, Int, Boolean)] =
          val out = collection.mutable.ListBuffer[(String, Int, Boolean)]()
          var i = 0
          while i < s.length do
            val ob = s.indexOf('[', i)
            if ob < 0 then i = s.length
            else
              val cb = s.indexOf(']', ob + 1)
              if cb < 0 then i = ob + 1
              else if cb + 1 < s.length && s.charAt(cb + 1) == '(' then
                var p = cb + 2
                var depth = 1
                val startContent = p
                var matched = false
                while p < s.length && !matched do
                  val ch = s.charAt(p)
                  if ch == '(' then depth += 1
                  else if ch == ')' then depth -= 1
                  if depth == 0 then matched = true
                  else p += 1
                if matched then
                  val content = s.substring(startContent, p)
                  out += ((content, startContent, false))
                  i = p + 1
                else
                  // Unclosed '(' — mark as malformed and report later
                  val content = s.substring(startContent)
                  out += ((content, startContent, true))
                  i = s.length
              else i = cb + 1
          out.toList

        for (symbolOrUrl, start) <- extractWikiLinks(block.content) do
          matches += ((symbolOrUrl, start))
        for (url, start, malformed) <- extractMdLinks(wikidocMasked) do
          if malformed then
            // Report malformed markdown link immediately
            val before = block.content.substring(0, start)
            val offsetLines = before.count(_ == '\n')
            val linkLine = block.lineNumber + offsetLines
            results += ((path.toString, linkLine, url, "Malformed link: missing closing ')'"))
          else
            matches += ((url, start))
        for m <- urlRegex.findAllMatchIn(wikidocMasked) do
          matches += ((m.matched, m.start))

        // Deduplicate by (url, start) if needed and check each
        for (url, startIdx) <- matches.distinct do
          val before = block.content.substring(0, startIdx)
          val offsetLines = before.count(_ == '\n')
          val linkLine = block.lineNumber + offsetLines
          checkUrl(url) match
            case Some(err) => results += ((path.toString, linkLine, url, err))
            case None      => ()

    results.toList

  /** Find broken HTTP/HTTPS links with external mappings support.
    *
    *  @param root Root directory to scan
    *  @param externalMappings Optional list of external documentation mappings
    *  @return List of broken links with (filePath, lineNumber, url, errorDescription)
    */
  def findBrokenLinks(root: Path, externalMappings: List[ExternalDocLink]): List[(String, Int, String, String)] =
    findBrokenLinks(root, (url: String) => defaultCheckUrl(url, root, externalMappings))

  /** Default network-backed findBrokenLinks implementation (kept for normal usage). */
  def findBrokenLinks(root: Path): List[(String, Int, String, String)] =
    findBrokenLinks(root, Nil)

  /** Default URL checker that supports external mappings.
    *
    *  @param url The URL or symbol to check
    *  @param root Root directory for source file lookup
    *  @param externalMappings Optional list of external documentation mappings
    *  @return None if URL is valid, Some(errorDescription) if invalid
    */
  private def defaultCheckUrl(u: String, root: Path, externalMappings: List[ExternalDocLink]): Option[String] =
      // Treat http(s) URLs via network check
      if u.startsWith("http://") || u.startsWith("https://") then
        var conn: HttpURLConnection | Null = null
        try
          val connection = URL(u).openConnection().asInstanceOf[HttpURLConnection]
          conn = connection
          connection.setConnectTimeout(5000)
          connection.setReadTimeout(5000)
          connection.setInstanceFollowRedirects(true)
          connection.setRequestProperty("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/115.0")
          // Try HEAD first; fallback to GET if server rejects HEAD
          try
            connection.setRequestMethod("HEAD")
            connection.connect()
            val code = connection.getResponseCode
            if code >= 400 then Some(s"HTTP $code") else None
          catch
            case _: java.net.ProtocolException =>
              // HEAD not supported; try GET
              connection.disconnect()
              val gconn = URL(u).openConnection().asInstanceOf[HttpURLConnection]
              conn = gconn
              gconn.setConnectTimeout(5000)
              gconn.setReadTimeout(5000)
              gconn.setInstanceFollowRedirects(true)
              gconn.setRequestProperty("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/115.0")
              gconn.setRequestMethod("GET")
              gconn.connect()
              val gcode = gconn.getResponseCode
              if gcode >= 400 then Some(s"HTTP $gcode") else None
        catch
          case e: Exception =>
            Some(s"Error: ${e.getMessage}")
        finally
          if conn != null then try conn.disconnect() catch case _: Throwable => ()
      else
        // Non-http link — treat as a code symbol reference like "scala.Int" or "com.example.Foo".
        // Consider common Scala primitive aliases as valid.
        val knownScalaPrimitives = Set(
          "scala.Int", "scala.Long", "scala.Double", "scala.Float",
          "scala.Boolean", "scala.Char", "scala.Unit", "scala.Byte",
          "scala.Short", "scala.Any", "scala.AnyRef", "scala.Nothing",
          "scala.Null", "scala.Predef.String", "scala.String"
        )
        if knownScalaPrimitives.contains(u) then
          None
        else
          // Normalize symbol by stripping parameter lists and generic type arguments
          // from the last segment so method signatures like `Predef.print(x:Any)` become `Predef.print`.
          def normalizeSymbol(sym: String): String =
            val s0 = sym.trim
            val noParams = s0.replaceAll(raw"\([^\)]*\)", "")
            val noGenerics = noParams.replaceAll(raw"\[[^\]]*\]", "")
            noGenerics.trim

          // Unescape backslash-dot sequences (e.g., scala\.collection\.mutable -> scala.collection.mutable)
          val unescaped = u.replace("\\.", ".")
          val normalized = normalizeSymbol(unescaped)

          // Extract parameter specification if present (handles unclosed paren too).
          val paramPattern = "\\(([^)]*)\\)".r
          val unclosedParamPattern = "\\(([^)]*)$".r
          val paramSpecOpt: Option[String] =
            paramPattern.findFirstMatchIn(unescaped).map(_.group(1)).orElse(unclosedParamPattern.findFirstMatchIn(unescaped).map(_.group(1)))

          // Extract and validate types in parameter lists
          // For example, from "(i:Iterato[A], j:List[B])" extract ["Iterato[A]", "List[B]"]
          def extractTypesFromParams(paramSpec: String): List[String] =
            // Split by comma first, then extract type after colon
            paramSpec.split(',').toList.flatMap { param =>
              val trimmed = param.trim
              val colonIdx = trimmed.indexOf(':')
              if colonIdx >= 0 then
                var typeStr = trimmed.substring(colonIdx + 1).trim
                // Remove trailing varargs marker or other modifiers
                typeStr = typeStr.replaceAll("\\s*\\*\\s*$", "")
                if typeStr.nonEmpty then Some(typeStr) else None
              else None
            }

          def validateType(typeStr: String): Option[String] =
            // Unescape backslash-dot sequences (e.g., scala\.collection\.mutable -> scala.collection.mutable)
            val unescapedType = typeStr.replace("\\.", ".")
            // Normalize type by removing generic parameters
            val baseType = unescapedType.replaceAll("\\[.*\\]", "").trim
            // Check if it's a known primitive (qualified or unqualified)
            if knownScalaPrimitives.contains(baseType) || knownScalaPrimitives.contains("scala." + baseType) then
              None
            else
              // Try to resolve via reflection or source lookup
              val normalizedType = normalizeSymbol(baseType)
              try
                // First try the type as-is
                if symbolResolvableByReflection(normalizedType) || symbolExistsInSource(root, normalizedType) then
                  None
                // For unqualified types (no dot), try common Scala package prefixes
                else if !normalizedType.contains('.') then
                  val commonPrefixes = List(
                    "scala.collection.",
                    "scala.collection.immutable.",
                    "scala.collection.mutable.",
                    "scala.util.",
                    "scala.concurrent.",
                    "scala.reflect.",
                    "scala.",
                    ""
                  )
                  val found = commonPrefixes.exists { prefix =>
                    val qualified = if prefix.isEmpty then normalizedType else prefix + normalizedType
                    symbolResolvableByReflection(qualified) || symbolExistsInSource(root, qualified)
                  }
                  if found then None else Some(s"Type not found: $unescapedType")
                else
                  Some(s"Type not found: $unescapedType")
              catch
                case e: Exception => Some(s"Error validating type $unescapedType: ${e.getMessage}")

          // Resolve member references like "AsJavaConverters.asJava" by finding the containing type
          // and searching for the member in its source file
          def memberExistsInSource(memberRef: String): Boolean =
            // Find the dot that separates the containing type from the member name
            // We need to find a dot that's not inside brackets [...] or parentheses (...)
            // For example, in "AsJavaConverters.asJava[A](b:scala.collection.mutable.Buffer[A])*"
            // we want to split at the dot after "AsJavaConverters", not after "mutable"
            var bracketDepth = 0
            var parenDepth = 0
            var splitIdx = -1
            var i = 0
            while i < memberRef.length && splitIdx < 0 do
              memberRef.charAt(i) match
                case '[' => bracketDepth += 1
                case ']' => bracketDepth -= 1
                case '(' => parenDepth += 1
                case ')' => parenDepth -= 1
                case '.' =>
                  if bracketDepth == 0 && parenDepth == 0 then
                    splitIdx = i
                case _ => ()
              i += 1

            if splitIdx < 0 then false
            else
              val containingType = memberRef.substring(0, splitIdx)
              val memberName = memberRef.substring(splitIdx + 1)
              // Normalize member name (remove generics, params, and trailing varargs marker)
              val normalizedMember = normalizeSymbol(memberName).replaceAll("\\*\\s*$", "")

              // Find the source file for the containing type
              val files = findScalaFiles(root)
              val pkgPattern = raw"""(?m)^\s*package\s+([^\s;]+)""".r
              val typeName = containingType.split('.').last
              val typeDeclPattern = raw"""(?m)^\s*(?:class|object|trait|type|case\s+class)\s+%s\b""".format(java.util.regex.Pattern.quote(typeName)).r

              files.exists { file =>
                try
                  val txt = Files.readString(file)
                  val pkgOpt = pkgPattern.findFirstMatchIn(txt).map(_.group(1))

                  // Check if this file declares the containing type
                  val containsType = if containingType.contains('.') then
                    // Fully qualified: check if package matches
                    pkgOpt.exists(pkg => pkg + "." + typeName == containingType) && typeDeclPattern.findFirstIn(txt).isDefined
                  else
                    // Unqualified: just check if the type is declared in this file
                    typeDeclPattern.findFirstIn(txt).isDefined

                  if containsType then
                    // Search for the member declaration in this file
                    // Look for: def, val, var, type, or object declarations with the member name
                    val memberDeclPattern = raw"""(?m)^\s*(?:def|val|var|type|object|lazy\s+val)\s+%s\b""".format(java.util.regex.Pattern.quote(normalizedMember)).r
                    memberDeclPattern.findFirstIn(txt).isDefined
                  else
                    false
                catch
                  case _: Throwable => false
              }
 
          // Enhanced reflection: handle fully-qualified classes, companion objects and simple object members.
          // When a param spec is present and contains a varargs marker ('*'), prefer methods that are varargs.
          def symbolResolvableByReflection(sym: String): Boolean =
            def methodMatches(md: java.lang.reflect.Method, paramSpec: Option[String]): Boolean =
              paramSpec match
                case Some(spec) =>
                  val wantsVarargs = spec.contains("*")
                  if wantsVarargs then md.isVarArgs else true
                case None => true

            try
              Class.forName(sym)
              true
            catch
              case _: ClassNotFoundException =>
                if sym.contains('.') then
                  val parts = sym.split('.')
                  val parent = parts.init.mkString(".")
                  val member = parts.last

                  // First, try to resolve as a member of a regular Java class
                  try
                    val parentClass = Class.forName(parent)
                    // First check if it's a nested class (e.g., java.util.Spliterator.OfInt -> java.util.Spliterator$OfInt)
                    val nestedClassName = parent + "$" + member
                    val hasNestedClass =
                      try
                        Class.forName(nestedClassName)
                        true
                      catch
                        case _: ClassNotFoundException => false

                    if hasNestedClass then true
                    else
                      // Check for fields
                      val hasField =
                        try { parentClass.getDeclaredField(member); true }
                        catch { case _: Throwable => false }
                      // Check for methods
                      val methods = parentClass.getMethods.filter(_.getName == member)
                      val hasMethod = methods.exists(m => methodMatches(m, paramSpecOpt))
                      hasField || hasMethod
                  catch
                    case _: ClassNotFoundException =>
                      // Not a regular Java class, try Scala companion object
                      try
                        val base = if parent.endsWith("$") then parent else parent + "$"
                        val candidates =
                          if parent.contains('.') then List(base)
                          else List(base, "scala." + base)
                        candidates.exists { companionClassName =>
                          try
                            val companionClass = Class.forName(companionClassName)
                            val moduleField = companionClass.getField("MODULE$")
                            val instance = moduleField.get(null)
                            val instClass = instance.getClass
                            // Check for a declared field or methods with the member name (and params match)
                            val hasField =
                              try { instClass.getDeclaredField(member); true }
                              catch { case _: Throwable => false }
                            val methods = instClass.getMethods.filter(_.getName == member)
                            val hasMethod = methods.exists(m => methodMatches(m, paramSpecOpt))
                            hasField || hasMethod
                          catch
                            case _: Throwable => false
                        }
                      catch
                        case _: Throwable => false
                else
                  // No dot — try common containers (scala.Predef) for unqualified member names
                  try
                    // Try scala.<Sym> class first
                    Class.forName("scala." + sym)
                    true
                  catch
                    case _: Throwable =>
                      // Check Predef module for methods matching the name
                      try
                        val companionClass = Class.forName("scala.Predef$")
                        val moduleField = companionClass.getField("MODULE$")
                        val instance = moduleField.get(null)
                        val instClass = instance.getClass
                        val methods = instClass.getMethods.filter(_.getName == sym)
                        methods.exists(m => methodMatches(m, paramSpecOpt))
                      catch
                        case _: Throwable => false
              case _: Throwable => false
 
          try
            // Always validate types in parameter lists, regardless of main symbol validity
            val typeError = paramSpecOpt match
              case Some(paramSpec) =>
                val types = extractTypesFromParams(paramSpec)
                types.map(validateType).find(_.isDefined).flatten
              case None => None

            // Check if this is a member reference (has dots)
            // A member reference is anything with a dot (e.g., Container.asJava, AsJavaConverters.asJava[A](...))
            val isMemberRef = unescaped.contains('.')

            // Then check the main symbol
            // First, try to resolve as a fully qualified type (via reflection or source lookup)
            // Then check if it's a package, and finally treat it as a member reference
            val symbolError =
              if symbolResolvableByReflection(normalized) then
                None
              else if symbolExistsInSource(root, normalized) then
                None
              else if isMemberRef && packageExistsInSource(root, normalized) then
                // It's a valid package
                None
              else if externalMappings.nonEmpty && ExternalDocLink.findMapping(normalized, externalMappings).isDefined then
                // The symbol is covered by external mappings
                None
              else if isMemberRef then
                // For member references, try to resolve by finding the containing type and searching for the member
                if memberExistsInSource(unescaped) then None
                else
                  // Try to resolve using external mappings
                  // Split the member reference into containing type and member name
                  // For example, "java.util.List.add" -> "java.util.List" and "add"
                  var bracketDepth = 0
                  var parenDepth = 0
                  var splitIdx = -1
                  var i = 0
                  while i < unescaped.length && splitIdx < 0 do
                    unescaped.charAt(i) match
                      case '[' => bracketDepth += 1
                      case ']' => bracketDepth -= 1
                      case '(' => parenDepth += 1
                      case ')' => parenDepth -= 1
                      case '.' =>
                        if bracketDepth == 0 && parenDepth == 0 then
                          splitIdx = i
                      case _ => ()
                    i += 1

                  if splitIdx >= 0 then
                    val containingType = unescaped.substring(0, splitIdx)
                    val memberName = unescaped.substring(splitIdx + 1)
                    val normalizedMember = normalizeSymbol(memberName).replaceAll("\\*\\s*$", "")

                    // Check if the containing type matches an external mapping
                    ExternalDocLink.findMapping(containingType, externalMappings) match
                      case Some(mapping) =>
                        // The type is covered by external mappings, so consider it valid
                        None
                      case None =>
                        Some("Member not found")
                  else
                    Some("Member not found")
              else
                Some("Symbol not found")

            // Report type error first if exists, otherwise report symbol error
            typeError match
              case Some(err) => Some(err)
              case None => symbolError
          catch
            case e: Exception => Some(s"Error: ${e.getMessage}")

  /** Generate summary statistics from file results. */
  def summarize(results: List[FileResult]): Summary =
    var defTotal = 0
    var defWithIssues = 0
    var classTotal = 0
    var classWithIssues = 0
    var traitTotal = 0
    var traitWithIssues = 0
    var totalIssues = 0

    for
      fileResult <- results
      checkResult <- fileResult.results
    do
      val hasIssues = checkResult.issues.nonEmpty
      totalIssues += checkResult.issues.size

      checkResult.declaration.kind match
        case DeclKind.Def =>
          defTotal += 1
          if hasIssues then defWithIssues += 1
        case DeclKind.Class =>
          classTotal += 1
          if hasIssues then classWithIssues += 1
        case DeclKind.Trait =>
          traitTotal += 1
          if hasIssues then traitWithIssues += 1
        case _ => ()

    Summary(
      totalFiles = results.size,
      totalIssues = totalIssues,
      defTotal = defTotal,
      defWithIssues = defWithIssues,
      classTotal = classTotal,
      classWithIssues = classWithIssues,
      traitTotal = traitTotal,
      traitWithIssues = traitWithIssues
    )

  /** Format results as a human-readable report. */
  def formatReport(results: List[FileResult], summary: Summary): String =
    val sb = new StringBuilder

    // Report issues per file
    for fileResult <- results if fileResult.hasIssues do
      sb.append(s"\nFile: ${fileResult.path}\n")
      for checkResult <- fileResult.results if checkResult.issues.nonEmpty do
        val decl = checkResult.declaration
        val line = checkResult.scaladoc.lineNumber
        sb.append(s"  At line $line: ${decl.kind.toString.toLowerCase} ${decl.name}\n")
        for issue <- checkResult.issues do
          sb.append(s"    - ${issue.message}\n")

    // Summary
    sb.append("\nSummary:\n")
    def pct(bad: Int, total: Int): String =
      if total > 0 then f"${bad.toDouble / total * 100}%.1f%%"
      else "0.0%"

    sb.append(s"  Methods: ${summary.defTotal}, with issues: ${summary.defWithIssues} (${pct(summary.defWithIssues, summary.defTotal)})\n")
    sb.append(s"  Classes: ${summary.classTotal}, with issues: ${summary.classWithIssues} (${pct(summary.classWithIssues, summary.classTotal)})\n")
    sb.append(s"  Traits: ${summary.traitTotal}, with issues: ${summary.traitWithIssues} (${pct(summary.traitWithIssues, summary.traitTotal)})\n")
    sb.append(s"\nScanned ${summary.totalFiles} files. Found ${summary.totalIssues} issues.\n")

    sb.toString
