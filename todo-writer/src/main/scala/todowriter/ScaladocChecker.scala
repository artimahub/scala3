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
