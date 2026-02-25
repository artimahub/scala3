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
  // Cache for package declarations to avoid repeated regex scanning
  // Maps file path to its full package name (with chaining support)
  private val packageCache = collection.mutable.Map[String, Option[String]]()
  
  // Cache for Scala files list to avoid repeated directory walks
  // Maps root path to its list of Scala files
  private val scalaFilesCache = collection.mutable.Map[String, List[Path]]()
  
  // Cache for file contents to avoid repeated disk reads
  // Maps file path to its content
  private val fileContentCache = collection.mutable.Map[String, String]()
  
  // Cache for type existence checks to avoid repeated regex searches
  // Maps type name to whether it exists in source
  private val typeExistsCache = collection.mutable.Map[String, Boolean]()

  /** Clear all caches. Call this before running tests with different root directories. */
  def clearCaches(): Unit =
    packageCache.clear()
    scalaFilesCache.clear()
    fileContentCache.clear()
    typeExistsCache.clear()

  /** Get file content with caching. */
  private def getFileContent(file: Path): String =
    val pathStr = file.toString
    fileContentCache.getOrElseUpdate(pathStr,
      try Files.readString(file)
      catch case _: Throwable => ""
    )

  /** Get the full package name for a file (with chaining support and caching).
    *  Returns the full package name like "scala.jdk.javaapi" for chained declarations.
    */
  private def getPackageName(file: Path): Option[String] =
    val pathStr = file.toString
    packageCache.getOrElseUpdate(pathStr,
      try
        val txt = getFileContent(file)
        // Skip package object declarations - they should not be treated as packages
        val pkgPattern = raw"""(?m)^\s*package\s+(?!object\s+)([^\s;\n]+)""".r
        val pkgMatch = pkgPattern.findFirstMatchIn(txt)
        if pkgMatch.isDefined then
          val firstMatch = pkgMatch.get
          val firstPkg = firstMatch.group(1)
          val afterFirstPkg = txt.substring(firstMatch.end)
          // Look for more package declarations immediately following
          val pkgPattern2 = raw"""(?m)^\s*package\s+(?!object\s+)([^\s;\n]+)""".r
          pkgPattern2.findFirstMatchIn(afterFirstPkg) match
            case Some(secondPkgMatch) =>
              val secondPkg = secondPkgMatch.group(1)
              val afterSecondPkg = afterFirstPkg.substring(secondPkgMatch.end)
              // Look for third package declaration
              pkgPattern2.findFirstMatchIn(afterSecondPkg) match
                case Some(thirdPkgMatch) =>
                  Some(firstPkg + "." + secondPkg + "." + thirdPkgMatch.group(1))
                case None =>
                  Some(firstPkg + "." + secondPkg)
            case None =>
              Some(firstPkg)
        else
          None
      catch
        case _: Throwable => None
    )

  /** Find all .scala files recursively under the given directory. */
  def findScalaFiles(root: Path): List[Path] =
    if !Files.isDirectory(root) then Nil
    else
      val rootStr = root.toString
      scalaFilesCache.getOrElseUpdate(rootStr,
        Files
          .walk(root)
          .iterator()
          .asScala
          .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".scala"))
          .toList
      )

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
    *  Note: For test isolation, consider calling clearCaches() before calling this function
    *  if running multiple tests with different root directories.
    */
  def symbolExistsInSource(root: Path, symbol: String): Boolean =
    // symbol like "com.example.Foo" -> pkg="com.example", name="Foo"
    val parts = symbol.split('.')
    if parts.isEmpty then false
    else
      val name = parts.last
      val files = findScalaFiles(root)
      val declPattern = raw"""(?m)^\s*(?:class|object|trait|type|case\s+class)\s+%s\b""".format(java.util.regex.Pattern.quote(name)).r
      val pkgPattern = raw"""(?m)^\s*package\s+([^\s;\n]+)""".r
      val pkgObjectPattern = raw"""(?m)^\s*package\s+object\s+([^\s;\n]+)""".r
      files.exists { p =>
        try
          val txt = getFileContent(p)
          // Use cached package lookup
          val pkgOpt = getPackageName(p)

          // First, check for simple name match (unqualified reference)
          // This allows "ExecutionContext" to match regardless of package
          // Useful for same-package references like ExecutionContext.Implicits.global
          val simpleNameMatch = declPattern.findFirstIn(txt).isDefined
          if !symbol.contains('.') && simpleNameMatch then
            true
          else
            // Check if it's a regular package member with full qualified name
            val fullNameMatches =
              pkgOpt match
                case Some(pkg) => pkg + "." + name == symbol && declPattern.findFirstIn(txt).isDefined
                case None => declPattern.findFirstIn(txt).isDefined && name == symbol

            if fullNameMatches then true
            else
              // Also check inside package objects
              // For example, scala.BigInt is defined in package object scala
              if symbol.contains('.') then
                val pkgName = symbol.substring(0, symbol.lastIndexOf('.'))
                val memberName = name
                // Look for a package object that matches the package name
                val pkgObjectMatch = pkgObjectPattern.findFirstMatchIn(txt)
                if pkgObjectMatch.isDefined && pkgObjectMatch.get.group(1) == pkgName then
                  // Found a package object matching the package name, check for the member
                  declPattern.findFirstIn(txt).isDefined
                else
                  false
              else
                // Fallback for unqualified name (redundant but kept for safety)
                simpleNameMatch
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
    // 4. package scala followed by package util.hashing (package chaining with dots)
    val parts = pkg.split('.')
    files.exists { p =>
      try
        val txt = getFileContent(p)
        // Extract all package declarations from the file
        val pkgPattern = raw"""(?m)^\s*package\s+([^\s;\n]+)""".r
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
            // Check for package chaining (e.g., package scala followed by package util.hashing)
            // Concatenate consecutive package declarations with dots
            val concatenated = pkgs.mkString(".")
            if concatenated == pkg then true
            else
              // Check for package object (e.g., package scala followed by package object math)
              val parent = parts.init.mkString(".")
              val child = parts.last
              pkgs.contains(parent) && packageObjects.contains(child)
      catch
        case _: Throwable => false
    }

  /** Check if a member exists in source files with a given root path (recursive version for deeply nested objects).
    *  This is a helper that can be called from outside the defaultCheckUrl function.
    *  Handles deeply nested objects by recursively searching through the structure.
    */
  private def memberExistsInSourceRecursive(memberRef: String, rootPath: Path): Boolean =
    // For deeply nested objects like "scala.language.experimental.captureChecking",
    // we need to recursively search through the structure.
    // Split the reference into parts: ["scala", "language", "experimental", "captureChecking"]
    val parts = memberRef.split('.').toList
    if parts.length < 2 then false
    else
      // Try to find the first part as a top-level type, then recursively search for the rest
      findMemberInType(parts, rootPath)

  /** Recursively search for a member path through the type hierarchy.
    *  parts: ["scala", "language", "experimental", "captureChecking"]
    *  Returns true if "captureChecking" exists in "experimental" which exists in "language" which exists in package "scala"
    *
    *  For references like ["scala", "collection", "MapOps", "LazyKeySet"]:
    *  - First, find the longest prefix that matches a package (e.g., "scala.collection")
    *  - Then find the type in that package (e.g., "MapOps")
    *  - Then search for the rest of the path inside that type (e.g., "LazyKeySet")
    */
  private def findMemberInType(parts: List[String], rootPath: Path): Boolean =
    parts match
      case Nil => false
      case name :: Nil =>
        // Single component - check if it exists as a top-level type
        symbolExistsInSource(rootPath, name)
      case _ =>
        // Multiple components - try to find the longest prefix that matches a package
        // We need to find where the package ends and the type begins
        // Try different splits: [pkg, type, rest], [pkg.subpkg, type, rest], etc.
        var found = false
        var i = parts.length - 2
        while !found && i >= 1 do
          val pkgParts = parts.take(i)
          val typeName = parts(i)
          val rest = parts.drop(i + 1)

          val pkg = pkgParts.mkString(".")
          val fullName = pkg + "." + typeName

          // Check if this type exists as a top-level declaration
          if typeExistsInSource(rootPath, fullName) then
            // Type exists, now search for the rest of the path inside this type
            found = findMemberInTypeSource(fullName, rest, rootPath)
            // Always decrement i to try the next split if not found
            if !found then i -= 1
          else
            // Try the next split
            i -= 1

        // If we didn't find it with any split, try the fallback logic
        if !found && parts.length >= 2 then
          // Fallback 1: Try treating the reference as "package.Type.member" where "package.Type" is the full type name
          // For "ExecutionContext.Implicits.global", try to find type "ExecutionContext" first,
          // then search for "Implicits.global" inside it. This handles same-package references
          // where the file is in the same package as the type.
          val firstComponent = parts.head
          val rest = parts.tail

          // First, try to find the first component as a standalone type
          // This checks all packages for a type named "ExecutionContext" by simple name
          if symbolExistsInSource(rootPath, firstComponent) then
            found = findMemberInTypeSource(firstComponent, rest, rootPath)

          // Fallback 2: Try the original logic for package-based lookup
          if !found then
            val pkgName = parts.init.mkString(".")
            val typeName = parts.last
            found = findNestedTypeInPackage(pkgName, typeName, Nil, rootPath)

          found
        else
          found

  /** Check if a type exists in source files. */
  private def typeExistsInSource(rootPath: Path, typeName: String): Boolean =
    // Check cache first
    val cacheKey = rootPath.toString + ":" + typeName
    typeExistsCache.getOrElseUpdate(cacheKey, {
      val parts = typeName.split('.')
      val name = parts.last
      val pkg = parts.init.mkString(".")
      val files = findScalaFiles(rootPath)
      val pkgPattern = raw"""(?m)^\s*package\s+([^\s;\n]+)""".r
      val typeDeclPattern = raw"""(?m)^\s*(?:class|object|trait|type|case\s+class)\s+%s\b""".format(java.util.regex.Pattern.quote(name)).r

      files.exists { file =>
        try
          val txt = getFileContent(file)
          // Use cached package lookup
          val pkgOpt = getPackageName(file)
          pkgOpt.exists(p => p == pkg) && typeDeclPattern.findFirstIn(txt).isDefined
        catch
          case _: Throwable => false
      }
    })

  /** Find a member inside a specific type by reading its source file.
    *  This function handles nested objects by searching within the same source file
    *  for all levels of nesting.
    */
  private def findMemberInTypeSource(typeName: String, memberPath: List[String], rootPath: Path): Boolean =
    memberPath match
      case Nil => true  // Found everything
      case memberName :: rest =>
        // Find the file that declares the type
        val parts = typeName.split('.')
        val name = parts.last
        val pkg = parts.init.mkString(".")
        val files = findScalaFiles(rootPath)
        val pkgPattern = raw"""(?m)^\s*package\s+([^\s;\n]+)""".r
        val typeDeclPattern = raw"""(?m)^\s*(?:class|object|trait|type|case\s+class)\s+%s\b""".format(java.util.regex.Pattern.quote(name)).r

        files.exists { file =>
          try
            val txt = getFileContent(file)
            // Use cached package lookup
            val pkgOpt = getPackageName(file)

            // Check if this file contains the type
            // For qualified names (e.g., "scala.concurrent.ExecutionContext"), require package match
            // For unqualified names (e.g., "ExecutionContext"), just check the simple name
            val typeMatches =
              if typeName.contains('.') then
                // Qualified name: require both package and simple name to match
                pkgOpt.exists(p => p == pkg) && typeDeclPattern.findFirstIn(txt).isDefined
              else
                // Unqualified name: just check the simple name (any package)
                typeDeclPattern.findFirstIn(txt).isDefined

            if typeMatches then
              // Found the type, now search for the member inside it
              // Look for class, object, trait, def, val, var, type declarations with the member name
              // Include optional modifiers like implicit, final, private, protected, override, etc.
              // Modifiers can appear multiple times and in any order
              // Also handle access modifiers with brackets like private[collection], protected[This]
              val memberDeclPattern = raw"""(?m)^\s*(?:implicit|final|private|protected|override|abstract|sealed|lazy|open|transparent|inline|private\[.*?\]|protected\[.*?\]|\s+)*(?:class|object|trait|def|val|var|lazy\s+val|type)\s+%s\b""".format(java.util.regex.Pattern.quote(memberName)).r

              if memberDeclPattern.findFirstIn(txt).isDefined then
                // Found the member. Now we need to search for the rest of the path.
                // For deeply nested objects, we search within the same source file.
                // We need to find the member declaration and then search within its body.
                if rest.isEmpty then
                  true  // Found everything
                else
                  // Search for the rest of the path within the member's body
                  findMemberWithinBody(memberName, rest, txt)
              else
                false
            else
              false
          catch
            case _: Throwable => false
        }

  /** Find a member path within a body of code (e.g., inside an object definition).
    *  This handles deeply nested objects by recursively searching within nested bodies.
    *  @param currentMemberName The name of the current member we're searching inside
    *  @param remainingPath The remaining path to search (e.g., ["captureChecking"])
    *  @param txt The source text to search within
    *  @return true if the path is found
    */
  private def findMemberWithinBody(currentMemberName: String, remainingPath: List[String], txt: String): Boolean =
    remainingPath match
      case Nil => true  // Found everything
      case nextMember :: rest =>
        // Find the body of the current member
        // We need to locate the object/class definition and then search within its body
        // This is a simplified approach - we search for the member declaration anywhere
        // after the current member's declaration.

        // Find the position of the current member's declaration
        // Include optional modifiers like implicit, final, private, protected, override, etc.
        // Modifiers can appear multiple times and in any order
        // Also handle access modifiers with brackets like private[collection], protected[This]
        val memberDeclPattern = raw"""(?m)^\s*(?:implicit|final|private|protected|override|abstract|sealed|lazy|open|transparent|inline|private\[.*?\]|protected\[.*?\]|\s+)*(?:class|object|trait|def|val|var|lazy\s+val|type)\s+%s\b""".format(java.util.regex.Pattern.quote(currentMemberName)).r
        memberDeclPattern.findFirstMatchIn(txt) match
          case Some(m) =>
            // Get the text after this member's declaration
            val afterMember = txt.substring(m.end)

            // Look for the next member in the remaining path
            val nextMemberPattern = raw"""(?m)^\s*(?:implicit|final|private|protected|override|abstract|sealed|lazy|open|transparent|inline|private\[.*?\]|protected\[.*?\]|\s+)*(?:class|object|trait|def|val|var|lazy\s+val|type)\s+%s\b""".format(java.util.regex.Pattern.quote(nextMember)).r
            nextMemberPattern.findFirstMatchIn(afterMember) match
              case Some(_) =>
                // Found the next member, recursively search for the rest
                if rest.isEmpty then
                  true  // Found everything
                else
                  // Continue searching within the same body
                  findMemberWithinBody(nextMember, rest, afterMember)
              case None =>
                // Next member not found
                false
          case None =>
            // Current member not found
            false
      case _ =>
        false

  /** Find a nested type inside a package. */
  private def findNestedTypeInPackage(pkgName: String, typeName: String, memberPath: List[String], rootPath: Path): Boolean =
    // This is complex - for now, just try to find the type with the full name
    // The recursive search would require parsing the entire package structure
    // For @compileTimeOnly members, we rely on the fact that they are declared in source files
    typeExistsInSource(rootPath, pkgName + "." + typeName) && 
      findMemberInTypeSource(pkgName + "." + typeName, memberPath, rootPath)

  /** Find broken HTTP/HTTPS links referenced inside Scaladoc blocks using a custom checker.
    *
    *  Returns a list of tuples: (filePath, lineNumber, url, errorDescription)
    *  The lineNumber points to the Scaladoc physical line containing the link.
    *
    *  This variant is useful for testing (inject a fake checker). The checker
    *  should return None for a healthy URL, or Some(errorDescription) for a broken URL.
    *
    *  @param root Root directory to scan
    *  @param checkUrl Function to check if a URL is valid (receives URL and declaration context)
    *  @return List of broken links with (filePath, lineNumber, url, errorDescription)
    */
  def findBrokenLinks(root: Path, checkUrl: (String, Declaration) => Option[String]): List[(String, Int, String, String)] =
    findBrokenLinks(root, checkUrl, Nil)

  /** Find broken HTTP/HTTPS links referenced inside Scaladoc blocks using a custom checker with external mappings.
    *
    *  Returns a list of tuples: (filePath, lineNumber, url, errorDescription)
    *  The lineNumber points to the Scaladoc physical line containing the link.
    *
    *  @param root Root directory to scan
    *  @param checkUrl Function to check if a URL is valid (receives URL and declaration context)
    *  @param externalMappings Optional list of external documentation mappings
    *  @return List of broken links with (filePath, lineNumber, url, errorDescription)
    */
  private def findBrokenLinks(root: Path, checkUrl: (String, Declaration) => Option[String], externalMappings: List[ExternalDocLink]): List[(String, Int, String, String)] =
    // Clear all caches at the start of each run
    clearCaches()
    
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
        // Get the declaration that follows this Scaladoc block
        val chunk = Declaration.getDeclarationAfter(text, block.endIndex)
        val decl = Declaration.parse(chunk)

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
          checkUrl(url, decl) match
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
    findBrokenLinks(root, (url: String, decl: Declaration) => defaultCheckUrl(url, decl, root, externalMappings))

  /** Default network-backed findBrokenLinks implementation (kept for normal usage). */
  def findBrokenLinks(root: Path): List[(String, Int, String, String)] =
    findBrokenLinks(root, Nil)

  /** Check if a symbol name refers to a member of the current declaration.
    *
    *  @param symbolName The unqualified symbol name to check
    *  @param decl The declaration context
    *  @return true if the symbol is a member of the declaration
    */
  private def hasMemberInDeclaration(symbolName: String, decl: Declaration): Boolean =
    // Check if the symbol is a type parameter
    if decl.tparams.contains(symbolName) then true
    // Check if the symbol is a value parameter
    else if decl.params.contains(symbolName) then true
    // For classes and traits, also check constructor parameters (they're in params)
    else if decl.kind == DeclKind.Class || decl.kind == DeclKind.Trait then
      decl.params.contains(symbolName)
    else
      // For methods, we can't easily check if there are other methods in the same type
      // without parsing the entire class, so we conservatively return false
      // In practice, Scaladoc often uses unqualified references for methods within the same type
      // and the checker should not report them as broken
      false

  /** Default URL checker that supports external mappings.
    *
    *  @param url The URL or symbol to check
    *  @param decl The declaration context (for resolving unqualified references)
    *  @param root Root directory for source file lookup
    *  @param externalMappings Optional list of external documentation mappings
    *  @return None if URL is valid, Some(errorDescription) if invalid
    */
  private def defaultCheckUrl(u: String, decl: Declaration, root: Path, externalMappings: List[ExternalDocLink]): Option[String] =
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
          // Convert # to . for Scaladoc member references (e.g., scala.Option#flatten -> scala.Option.flatten)
          val withDotForHash = unescaped.replace("#", ".")
          val normalized = normalizeSymbol(withDotForHash)

          // Extract parameter specification if present (handles unclosed paren too).
          val paramPattern = "\\(([^)]*)\\)".r
          val unclosedParamPattern = "\\(([^)]*)$".r
          val paramSpecOpt: Option[String] =
            paramPattern.findFirstMatchIn(withDotForHash).map(_.group(1)).orElse(unclosedParamPattern.findFirstMatchIn(withDotForHash).map(_.group(1)))

          // Extract and validate types in parameter lists
          // For example, from "(i:Iterato[A], j:List[B])" extract ["Iterato[A]", "List[B]"]
          def extractTypesFromParams(paramSpec: String): List[String] =
            // Split by comma, but only at commas that are outside brackets [...] and parentheses (...)
            var result = List[String]()
            var currentParam = new StringBuilder()
            var bracketDepth = 0
            var parenDepth = 0
            
            for ch <- paramSpec do
              ch match
                case '[' => bracketDepth += 1; currentParam.append(ch)
                case ']' => bracketDepth -= 1; currentParam.append(ch)
                case '(' => parenDepth += 1; currentParam.append(ch)
                case ')' => parenDepth -= 1; currentParam.append(ch)
                case ',' =>
                  if bracketDepth == 0 && parenDepth == 0 then
                    // Comma at depth 0 - this is a parameter separator
                    result = result :+ currentParam.toString().trim
                    currentParam = new StringBuilder()
                  else
                    // Comma inside brackets or parentheses - part of the type
                    currentParam.append(ch)
                case _ => currentParam.append(ch)
            
            // Add the last parameter
            if currentParam.nonEmpty then
              result = result :+ currentParam.toString().trim
            
            // Extract type from each parameter (after the colon)
            result.flatMap { param =>
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
          def memberExistsInSourceLocal(memberRef: String): Boolean =
            // In Scaladoc, '!' serves as a separator between type and member signature
            // Check if there's a '!' separator first
            val bangIdx = memberRef.indexOf('!')

            val (containingType, memberName) = if bangIdx >= 0 then
              // Split at '!' - everything before is the type, everything after is the member
              val typePart = memberRef.substring(0, bangIdx)
              var memberPart = memberRef.substring(bangIdx + 1)
              // Remove leading dot if present (e.g., ".sizeCompare" -> "sizeCompare")
              if memberPart.startsWith(".") then
                memberPart = memberPart.substring(1)
              (typePart, memberPart)
            else
              // No '!' separator - use the old logic to find the dot that separates the type from the member
              // Find the dot that's not inside brackets [...] or parentheses (...)
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

              if splitIdx < 0 then (memberRef, "")
              else (memberRef.substring(0, splitIdx), memberRef.substring(splitIdx + 1))

            if memberName.isEmpty then false
            else
              // Normalize member name (remove generics, params, return type, and trailing varargs marker)
              var normalizedMember = normalizeSymbol(memberName).replaceAll("\\*\\s*$", "")
              // Remove return type annotation (e.g., "sizeCompare:Int" -> "sizeCompare")
              val colonIdx = normalizedMember.indexOf(':')
              if colonIdx >= 0 then
                normalizedMember = normalizedMember.substring(0, colonIdx)

              // Find the source file for the containing type
              val files = findScalaFiles(root)
              val pkgPattern = raw"""(?m)^\s*package\s+([^\s;\n]+)""".r
              val typeName = containingType.split('.').last
              // Include optional modifiers like implicit, final, private, protected, override, etc.
              // Modifiers can appear multiple times and in any order
              // Also handle access modifiers with brackets like private[collection], protected[This]
              val typeDeclPattern = raw"""(?m)^\s*(?:implicit|final|private|protected|override|abstract|sealed|lazy|open|transparent|inline|private\[.*?\]|protected\[.*?\]|\s+)*(?:class|object|trait|type|case\s+class)\s+%s\b""".format(java.util.regex.Pattern.quote(typeName)).r

              files.exists { file =>
                try
                  val txt = getFileContent(file)
                  // Use cached package lookup
                  val pkgOpt = getPackageName(file)

                  // Check if this file declares the containing type
                  val containsType = if containingType.contains('.') then
                    // Fully qualified: check if package matches
                    pkgOpt.exists(pkg => pkg + "." + typeName == containingType) && typeDeclPattern.findFirstIn(txt).isDefined
                  else
                    // Unqualified: just check if the type is declared in this file
                    typeDeclPattern.findFirstIn(txt).isDefined

                  if containsType then
                    // Search for the member declaration in this file
                    // Look for: class, object, trait, def, val, var, type declarations with the member name
                    // Include optional modifiers like implicit, final, private, protected, override, etc.
                    // Modifiers can appear multiple times and in any order
                    // Also handle access modifiers with brackets like private[collection], protected[This]
                    val memberDeclPattern = raw"""(?m)^\s*(?:implicit|final|private|protected|override|abstract|sealed|lazy|open|transparent|inline|private\[.*?\]|protected\[.*?\]|\s+)*(?:class|object|trait|def|val|var|lazy\s+val|type)\s+%s\b""".format(java.util.regex.Pattern.quote(normalizedMember)).r
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

                    // Also check if it's a nested object (e.g., scala.math.Ordering.Implicits -> scala.math.Ordering$Implicits$)
                    val nestedObjectName = parent + "$" + member + "$"
                    val hasNestedObject =
                      try
                        Class.forName(nestedObjectName)
                        true
                      catch
                        case _: ClassNotFoundException => false

                    if hasNestedClass || hasNestedObject then true
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
              else if !isMemberRef then
                // Unqualified reference - conservatively assume it might be valid
                // It could be a method within the same type, which we can't easily detect
                // without parsing the entire class
                None
              else if isMemberRef then
                // For member references, try to resolve by finding the containing type and searching for the member
                // Before that, also try to resolve as a type (in case it's a type with dots like scala.BigInt)
                if symbolExistsInSource(root, withDotForHash) then None
                else if memberExistsInSourceLocal(withDotForHash) then None
                else if memberExistsInSourceRecursive(withDotForHash, root) then None
                else
                  // Try to resolve using external mappings
                  // Split the member reference into containing type and member name
                  // For example, "java.util.List.add" -> "java.util.List" and "add"
                  var bracketDepth = 0
                  var parenDepth = 0
                  var splitIdx = -1
                  var i = 0
                  while i < withDotForHash.length && splitIdx < 0 do
                    withDotForHash.charAt(i) match
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
                    val containingType = withDotForHash.substring(0, splitIdx)
                    val memberName = withDotForHash.substring(splitIdx + 1)
                    val normalizedMember = normalizeSymbol(memberName).replaceAll("\\*\\s*$", "")

                    // Check if the containing type matches an external mapping
                    ExternalDocLink.findMapping(containingType, externalMappings) match
                      case Some(mapping) =>
                        // The type is covered by external mappings, so consider it valid
                        None
                      case None =>
                        // Try to find the member in source files (for @compileTimeOnly members)
                        if memberExistsInSourceRecursive(withDotForHash, root) then None
                        else
                          // Try adding scala. prefix for common Scala library references
                          // For example, "collection.JavaConverters" -> "scala.collection.JavaConverters"
                          val withScalaPrefix = "scala." + withDotForHash
                          if memberExistsInSourceRecursive(withScalaPrefix, root) then None
                          else
                            // Try resolving relative package references for jdk javaapi
                            // For example, in package scala.jdk, "javaapi.FutureConverters" -> "scala.jdk.javaapi.FutureConverters"
                            if withDotForHash.startsWith("javaapi.") then
                              val qualified = "scala.jdk." + withDotForHash
                              if symbolExistsInSource(root, qualified) then None
                              else Some("Member not found")
                            else
                              Some("Member not found")
                  else
                    // Single component - try source file lookup
                    if memberExistsInSourceRecursive(withDotForHash, root) then None
                    else
                      // Try adding scala. prefix for common Scala library references
                      val withScalaPrefix = "scala." + withDotForHash
                      if memberExistsInSourceRecursive(withScalaPrefix, root) then None
                      else Some("Member not found")
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
