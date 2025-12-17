package todowriter

import java.nio.file.{Files, Path}

/** Result of fixing a file. */
case class FixResult(
    path: String,
    blocksFixed: Int,
    newContent: Option[String]
)

object Fixer:
  /** Fix all issues in a file and return the result. */
  def fixFile(path: Path, results: List[CheckResult]): FixResult =
    val text = Files.readString(path)
    val (newText, fixCount) = applyFixes(text, results)

    if newText != text then
      FixResult(path.toString, fixCount, Some(newText))
    else
      FixResult(path.toString, 0, None)

  /** Write fixed content to file. */
  def writeFixedFile(path: Path, content: String): Unit =
    Files.writeString(path, content)

  /** Apply fixes to text and return (newText, numberOfBlocksFixed). */
  def applyFixes(text: String, results: List[CheckResult]): (String, Int) =
    // Sort results by start index descending so we can apply fixes from end to start
    // (this way indices don't shift as we make replacements)
    val sortedResults = results
      .filter(needsFix)
      .sortBy(_.scaladoc.startIndex)(Ordering[Int].reverse)

    var currentText = text
    var fixCount = 0

    for result <- sortedResults do
      val block = result.scaladoc
      val decl = result.declaration
      val issues = result.issues

      // Determine what tags need to be inserted
      val missingParams = issues.collect { case Issue.MissingParam(names) => names }.flatten
      val missingTparams = issues.collect { case Issue.MissingTparam(names) => names }.flatten
      val needsReturn = issues.contains(Issue.MissingReturn)

      if missingParams.nonEmpty || missingTparams.nonEmpty || needsReturn then
        val newBlock = buildFixedBlock(
          currentText,
          block,
          missingTparams,
          missingParams,
          needsReturn
        )
        currentText = currentText.substring(0, block.startIndex) +
          newBlock +
          currentText.substring(block.endIndex)
        fixCount += 1

    (currentText, fixCount)

  /** Check if a result needs fixing (has issues that require insertion). */
  private def needsFix(result: CheckResult): Boolean =
    result.issues.exists {
      case Issue.MissingParam(_)  => true
      case Issue.MissingTparam(_) => true
      case Issue.MissingReturn    => true
      case _                      => false
    }

  /** Build a fixed Scaladoc block with missing tags inserted and proper indentation. */
  def buildFixedBlock(
      text: String,
      block: ScaladocBlock,
      missingTparams: List[String],
      missingParams: List[String],
      needsReturn: Boolean
  ): String =
    // Determine leading whitespace from the original position
    val lineStartIdx = text.lastIndexOf('\n', block.startIndex - 1)
    val linePrefix =
      if lineStartIdx < 0 then text.substring(0, block.startIndex)
      else text.substring(lineStartIdx + 1, block.startIndex)
    val leadingWs = linePrefix.takeWhile(_.isWhitespace)

    // Parse existing content
    val inner = block.content
    val contentLines = parseScaladocContent(inner)

    // Build tags to insert
    val tagsToInsert = collection.mutable.ListBuffer[String]()
    for tp <- missingTparams do
      tagsToInsert += s"@tparam $tp TODO"
    for p <- missingParams do
      tagsToInsert += s"@param $p TODO"
    if needsReturn then
      tagsToInsert += "@return TODO"

    // Check if original was single-line
    val originalBlock = text.substring(block.startIndex, block.endIndex)
    val wasSingleLine = !originalBlock.contains('\n')

    if wasSingleLine && tagsToInsert.nonEmpty then
      // Convert single-line to multi-line
      buildMultiLineBlock(leadingWs, contentLines, tagsToInsert.toList)
    else if wasSingleLine then
      // Keep as single line (no changes needed, but we shouldn't get here)
      originalBlock
    else
      // Already multi-line, insert tags
      buildMultiLineBlock(leadingWs, contentLines, tagsToInsert.toList)

  /** Parse Scaladoc inner content into structured lines. */
  private case class ContentLine(
      isTag: Boolean,
      isBlank: Boolean,
      isCodeBlock: Boolean,
      content: String
  )

  private def parseScaladocContent(inner: String): List[ContentLine] =
    val lines = inner.linesIterator.toList
    var inCodeBlock = false
    var inPreBlock = false

    lines.map { line =>
      val trimmed = line.trim
      // Remove leading * if present
      val afterStar =
        if trimmed.startsWith("*") then trimmed.drop(1) else trimmed
      val content = afterStar.trim

      // Track code blocks
      if content.startsWith("{{{") then inCodeBlock = true
      if content.contains("}}}") then inCodeBlock = false
      if content.toLowerCase.startsWith("<pre>") then inPreBlock = true
      if content.toLowerCase.contains("</pre>") then inPreBlock = false

      val isInCode = inCodeBlock || inPreBlock
      val isTag = content.startsWith("@")
      val isBlank = content.isEmpty

      ContentLine(isTag, isBlank, isInCode, afterStar)
    }

  /** Build a properly formatted multi-line Scaladoc block. */
  private def buildMultiLineBlock(
      leadingWs: String,
      contentLines: List[ContentLine],
      tagsToInsert: List[String]
  ): String =
    val parts = collection.mutable.ListBuffer[String]()

    // Opening
    parts += "/**"

    // Process existing content lines
    var lastWasBlank = false
    for line <- contentLines do
      if line.isBlank then
        // Emit blank star line
        parts += s"$leadingWs *"
        lastWasBlank = true
      else if line.isCodeBlock then
        // Preserve code block content exactly
        parts += s"$leadingWs *${line.content}"
        lastWasBlank = false
      else
        // Normal content - ensure two spaces after *
        val content = line.content
        if content.startsWith(" ") then
          // Already has leading space(s), preserve them but ensure at least one
          parts += s"$leadingWs *$content"
        else
          // Add two spaces before content
          parts += s"$leadingWs * $content"
        lastWasBlank = false

    // Insert new tags
    for tag <- tagsToInsert do
      parts += s"$leadingWs *  $tag"

    // Closing
    parts += s"$leadingWs */"

    parts.mkString("\n")
