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
        val masked = maskCodeRegions(block.content)

        // Collect link matches from markdown [label](link) and raw http(s) URLs,
        // preserving the match start index so we can compute the physical line.
        val matches = collection.mutable.ListBuffer[(String, Int)]()
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
        for (url, start, malformed) <- extractMdLinks(masked) do
          if malformed then
            // Report malformed markdown link immediately
            val before = block.content.substring(0, start)
            val offsetLines = before.count(_ == '\n')
            val linkLine = block.lineNumber + offsetLines
            results += ((path.toString, linkLine, url, "Malformed link: missing closing ')'"))
          else
            matches += ((url, start))
        for m <- urlRegex.findAllMatchIn(masked) do
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

  /** Default network-backed findBrokenLinks implementation (kept for normal usage). */
  def findBrokenLinks(root: Path): List[(String, Int, String, String)] =
    // Default checker: HTTP(S) links are validated over the network; non-http links
    // are treated as code/symbol references and validated via class lookup, known
    // Scala primitives, or by scanning the project's source files.
    //
    // The helper below performs a best-effort lookup inside source files under `root`.

    def defaultCheck(u: String): Option[String] =
      // Treat http(s) URLs via network check
      if u.startsWith("http://") || u.startsWith("https://") then
        var conn: HttpURLConnection | Null = null
        try
          val connection = URL(u).openConnection().asInstanceOf[HttpURLConnection]
          conn = connection
          connection.setConnectTimeout(5000)
          connection.setReadTimeout(5000)
          connection.setInstanceFollowRedirects(true)
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

          val normalized = normalizeSymbol(u)
 
          // Extract parameter specification if present (handles unclosed paren too).
          val paramPattern = "\\(([^)]*)\\)".r
          val unclosedParamPattern = "\\(([^)]*)$".r
          val paramSpecOpt: Option[String] =
            paramPattern.findFirstMatchIn(u).map(_.group(1)).orElse(unclosedParamPattern.findFirstMatchIn(u).map(_.group(1)))
 
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
            if symbolResolvableByReflection(normalized) then None
            else if symbolExistsInSource(root, normalized) then None
            else Some("Symbol not found")
          catch
            case e: Exception => Some(s"Error: ${e.getMessage}")

    end defaultCheck

    findBrokenLinks(root, defaultCheck)

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
