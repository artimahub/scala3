package todowriter

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

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

    val undocumentedResults = findUndocumentedResults(text, blocks)

    FileResult(path.toString, results ++ undocumentedResults)

  /** Find declarations with no preceding Scaladoc block and return synthetic CheckResults. */
  private def findUndocumentedResults(text: String, existingBlocks: List[ScaladocBlock]): List[CheckResult] =
    // Compute covered declaration line-starts from existing Scaladoc blocks.
    val coveredLineStarts: Set[Int] = existingBlocks.flatMap { block =>
      firstNonBlankLineStart(text, block.endIndex)
    }.toSet

    val undocResults = collection.mutable.ListBuffer[CheckResult]()
    val textLen = text.length
    var pos = 0
    while pos < textLen do
      val lineStart = pos
      val lineEnd   = text.indexOf('\n', pos)
      val lineContent =
        if lineEnd >= 0 then text.substring(pos, lineEnd)
        else text.substring(pos)
      pos = if lineEnd >= 0 then lineEnd + 1 else textLen

      if !coveredLineStarts.contains(lineStart) then
        val trimmed = lineContent.trim
        if trimmed.nonEmpty && !trimmed.startsWith("//") &&
           !trimmed.startsWith("*") && !trimmed.startsWith("/*") then
          val chunk = Declaration.getDeclarationAfter(text, lineStart)
          val decl  = Declaration.parse(chunk)
          if (decl.kind == DeclKind.Def || decl.kind == DeclKind.Class || decl.kind == DeclKind.Trait) &&
             decl.name.nonEmpty &&
             declKeywordOnLine(trimmed, decl.kind) then
            val missingParams  = decl.params
            val missingTparams = decl.tparams
            val needsReturn =
              decl.kind == DeclKind.Def &&
              decl.returnType.exists(r => !r.trim.startsWith("Unit"))
            val issues = collection.mutable.ListBuffer[Issue]()
            if missingParams.nonEmpty  then issues += Issue.MissingParam(missingParams)
            if missingTparams.nonEmpty then issues += Issue.MissingTparam(missingTparams)
            if needsReturn             then issues += Issue.MissingReturn
            if issues.nonEmpty then
              val lineNumber = text.substring(0, lineStart).count(_ == '\n') + 1
              val syntheticBlock = ScaladocBlock(
                content    = "",
                startIndex = lineStart,
                endIndex   = lineStart,
                lineNumber = lineNumber,
                params     = Nil,
                tparams    = Nil,
                hasReturn  = false,
                isOneLiner = false,
                synthetic  = true
              )
              undocResults += CheckResult(syntheticBlock, decl, issues.toList)
    undocResults.toList

  /** Return the byte offset of the first non-blank line that begins after `fromIndex`. */
  private def firstNonBlankLineStart(text: String, fromIndex: Int): Option[Int] =
    val textLen = text.length
    var pos     = fromIndex
    while pos < textLen && text(pos) != '\n' do pos += 1
    if pos < textLen then pos += 1
    var result: Option[Int] = None
    while pos < textLen && result.isEmpty do
      val lineStart = pos
      val lineEnd   = text.indexOf('\n', pos)
      val lineContent =
        if lineEnd >= 0 then text.substring(pos, lineEnd) else text.substring(pos)
      if lineContent.trim.nonEmpty then result = Some(lineStart)
      else pos = if lineEnd >= 0 then lineEnd + 1 else textLen
    result

  /** Check that the declaration keyword for `kind` actually appears on the trimmed line. */
  private def declKeywordOnLine(trimmed: String, kind: DeclKind): Boolean =
    val keyword = kind match
      case DeclKind.Def   => "def "
      case DeclKind.Class => "class "
      case DeclKind.Trait => "trait "
      case _              => return false
    val stripped = trimmed
      .replaceAll("""(?:@[\w\(\)\s,."]+\s*)*""", "")
      .replaceAll("""(?:private\[[^\]]*\]|protected\[[^\]]*\]|private|protected|final|override|inline|implicit|given|export|opaque|sealed|abstract|lazy|case)\s+""", "")
      .trim
    stripped.startsWith(keyword) || stripped.startsWith("case " + keyword)

  def validate(block: ScaladocBlock, decl: Declaration): List[Issue] =
    val issues = collection.mutable.ListBuffer[Issue]()

    // Only validate @param and @tparam for def, class, trait
    if decl.kind == DeclKind.Def || decl.kind == DeclKind.Class || decl.kind == DeclKind.Trait then
      // Check @param
      val missingParams = decl.params.filterNot(block.params.contains)
      
      // Detect unknown params, including duplicates
      val declParamCounts = decl.params.groupBy(identity).view.mapValues(_.size).toMap.withDefaultValue(0)
      val blockParamCounts = block.params.groupBy(identity).view.mapValues(_.size).toMap
      val unknownParams = blockParamCounts.collect { case (param, count) if declParamCounts(param) < count =>
        param
      }.toList.distinct

      if missingParams.nonEmpty then
        issues += Issue.MissingParam(missingParams)
      if unknownParams.nonEmpty then
        issues += Issue.UnknownParam(unknownParams)

      // Check @tparam
      val missingTparams = decl.tparams.filterNot(block.tparams.contains)
      
      // Detect unknown tparams, including duplicates
      val declTparamCounts = decl.tparams.groupBy(identity).view.mapValues(_.size).toMap.withDefaultValue(0)
      val blockTparamCounts = block.tparams.groupBy(identity).view.mapValues(_.size).toMap
      val unknownTparams = blockTparamCounts.collect { case (tparam, count) if declTparamCounts(tparam) < count =>
        tparam
      }.toList.distinct

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
