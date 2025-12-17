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

  /** Parsed representation of Scaladoc content. */
  private case class ParsedScaladoc(
      initialText: Option[String],      // First line of description (goes on /** line)
      descriptionLines: List[String],   // Additional description lines
      blankLinesBeforeTags: Int,        // Number of blank lines before first tag
      existingTags: List[String]        // Existing tags, each may contain newlines for multi-line tags
  )

  /** Parse Scaladoc inner content into structured parts.
   *
   *  Multi-line tags (like @return or @example spanning multiple lines) are preserved
   *  by joining continuation lines with newlines.
   */
  private def parseScaladocContent(inner: String): ParsedScaladoc =
    val lines = inner.linesIterator.toList

    var initialText: Option[String] = None
    val descriptionLines = collection.mutable.ListBuffer[String]()
    val existingTags = collection.mutable.ListBuffer[String]()
    var blankLinesBeforeTags = 0
    var inTagSection = false
    var foundFirstContent = false
    var blankLineCount = 0

    for line <- lines do
      val trimmed = line.trim
      // Remove leading * if present, but preserve content after it
      // We use dropWhile to handle "* " or "*  " prefixes
      val afterStar =
        if trimmed.startsWith("*") then
          val afterAsterisk = trimmed.drop(1)
          // Preserve relative indentation: just drop the leading "* " or "*"
          if afterAsterisk.startsWith(" ") then afterAsterisk.drop(1)
          else afterAsterisk
        else trimmed

      val isTag = afterStar.trim.startsWith("@")
      val isBlank = afterStar.trim.isEmpty

      if isTag then
        if !inTagSection then
          // First tag - record blank lines before it
          blankLinesBeforeTags = blankLineCount
          inTagSection = true
        existingTags += afterStar.trim
      else if isBlank then
        if !inTagSection then
          blankLineCount += 1
          // Only add blank lines to description if we've seen content
          if foundFirstContent then
            descriptionLines += ""
        // else: blank lines in tag section are ignored (formatting rules say no blanks between tags)
      else
        // Content line
        if !inTagSection then
          if !foundFirstContent then
            // This is the initial text
            initialText = Some(afterStar.trim)
            foundFirstContent = true
            blankLineCount = 0
          else
            // Additional description
            descriptionLines += afterStar.trim
            blankLineCount = 0
        else
          // Continuation of the previous tag (multi-line tag content)
          if existingTags.nonEmpty then
            existingTags(existingTags.length - 1) = existingTags.last + "\n" + afterStar

    ParsedScaladoc(initialText, descriptionLines.toList, blankLinesBeforeTags, existingTags.toList)

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
    val parsed = parseScaladocContent(inner)

    // Build tags to insert
    val newTags = collection.mutable.ListBuffer[String]()
    for tp <- missingTparams do
      newTags += s"@tparam $tp TODO FILL IN"
    for p <- missingParams do
      newTags += s"@param $p TODO FILL IN"
    if needsReturn then
      newTags += "@return TODO FILL IN"

    // Check if original was single-line
    val originalBlock = text.substring(block.startIndex, block.endIndex)
    val wasSingleLine = !originalBlock.contains('\n')

    // Combine and sort all tags in proper order: @tparam, @param, @return
    val allTags = sortTags(parsed.existingTags ++ newTags.toList)

    buildFormattedBlock(leadingWs, parsed, allTags, wasSingleLine)

  /** Sort tags in proper order: @see first, then @tparam, then @param, then @return.
   *  Other tags (like @throws, @since, etc.) are preserved at the end.
   */
  private def sortTags(tags: List[String]): List[String] =
    def tagOrder(tag: String): Int =
      if tag.startsWith("@see") then 0
      else if tag.startsWith("@tparam") then 1
      else if tag.startsWith("@param") then 2
      else if tag.startsWith("@return") then 3
      else 4 // Other tags go last

    tags.sortBy(tagOrder)

  /** Build a properly formatted Scaladoc block.
   *
   *  Format rules:
   *  1. Initial text appears on same line as opening
   *  2. Continuation lines have * aligned under first * of opening
   *  3. Text after * starts with two spaces
   *  4. Blank line before tags section
   */
  private def buildFormattedBlock(
      leadingWs: String,
      parsed: ParsedScaladoc,
      sortedTags: List[String],
      wasSingleLine: Boolean
  ): String =
    val hasDescription = parsed.initialText.isDefined || parsed.descriptionLines.nonEmpty
    val hasTags = sortedTags.nonEmpty

    // Special case: single-line with no additional content needed
    if wasSingleLine && parsed.descriptionLines.isEmpty && sortedTags.isEmpty then
      parsed.initialText match
        case Some(text) => s"/** $text */"
        case None => "/** */"

    // Special case: single-line that needs to become multi-line
    else if wasSingleLine && (parsed.descriptionLines.nonEmpty || hasTags) then
      buildMultiLineOutput(leadingWs, parsed, sortedTags)

    // Already multi-line
    else
      buildMultiLineOutput(leadingWs, parsed, sortedTags)

  private def buildMultiLineOutput(
      leadingWs: String,
      parsed: ParsedScaladoc,
      sortedTags: List[String]
  ): String =
    val parts = collection.mutable.ListBuffer[String]()

    // Opening line with initial text
    parsed.initialText match
      case Some(text) =>
        parts += s"/** $text"
      case None =>
        // No initial text - this shouldn't happen for well-formed scaladoc
        // but handle it gracefully
        parts += "/**"

    // Additional description lines
    for line <- parsed.descriptionLines do
      if line.isEmpty then
        parts += s"$leadingWs *"
      else
        parts += s"$leadingWs *  $line"

    // Blank line before tags (if there are tags and we need one)
    if sortedTags.nonEmpty then
      // Check if we need to add a blank line
      val lastLineIsBlank = parsed.descriptionLines.lastOption.contains("") ||
        (parsed.descriptionLines.isEmpty && parsed.initialText.isEmpty)
      val hadBlankBeforeTags = parsed.blankLinesBeforeTags > 0

      if !lastLineIsBlank && !hadBlankBeforeTags then
        parts += s"$leadingWs *"

    // All tags (already sorted: @see, @tparam, @param, @return)
    // Tags may contain newlines for multi-line content
    for tag <- sortedTags do
      val tagLines = tag.split("\n", -1) // -1 to keep trailing empty strings
      for (line, idx) <- tagLines.zipWithIndex do
        if idx == 0 then
          // First line starts with @tag
          parts += s"$leadingWs *  $line"
        else if line.isEmpty then
          // Blank line within tag content
          parts += s"$leadingWs *"
        else
          // Continuation line - preserve its content
          parts += s"$leadingWs *  $line"

    // Closing
    parts += s"$leadingWs */"

    parts.mkString("\n")
