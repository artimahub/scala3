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

  /** Parsed representation of Scaladoc content.
   *
   *  Scaladoc has two logical sections:
   *  1. Exposition: text paragraphs + exposition tags (@note, @see, @example)
   *  2. Signature: @tparam, @param, @return tags
   */
  private case class ParsedScaladoc(
      initialText: Option[String],           // First line of description (goes on /** line)
      expositionContent: List[String],       // Additional description lines and exposition tags
      signatureTags: List[String],           // Signature tags (@tparam, @param, @return)
      hasBlankBeforeSignature: Boolean       // Whether there was a blank line before signature tags
  )

  /** Tags that are part of the exposition (main content). */
  private val ExpositionTags = Set("@note", "@see", "@example")

  /** Tags that document the signature (appear at end). */
  private val SignatureTags = Set("@tparam", "@param", "@return")

  /** Check if a line starts with an exposition tag. */
  private def isExpositionTag(line: String): Boolean =
    ExpositionTags.exists(tag => line.startsWith(tag))

  /** Check if a line starts with a signature tag. */
  private def isSignatureTag(line: String): Boolean =
    SignatureTags.exists(tag => line.startsWith(tag))

  /** Parse Scaladoc inner content into structured parts.
   *
   *  Separates exposition content (text + @note/@see/@example) from
   *  signature documentation (@tparam/@param/@return).
   *  Multi-line tags are preserved by joining continuation lines with newlines.
   */
  private def parseScaladocContent(inner: String): ParsedScaladoc =
    val lines = inner.linesIterator.toList

    var initialText: Option[String] = None
    val expositionContent = collection.mutable.ListBuffer[String]()
    val signatureTags = collection.mutable.ListBuffer[String]()
    var foundFirstContent = false
    var pendingBlankLines = 0  // Track blank lines to add before next content
    var inSignatureSection = false
    var hasBlankBeforeSignature = false
    var currentTag: Option[collection.mutable.StringBuilder] = None
    var currentTagIsSignature = false

    def flushCurrentTag(): Unit =
      currentTag.foreach { tag =>
        val tagStr = tag.toString
        if currentTagIsSignature then
          signatureTags += tagStr
        else
          expositionContent += tagStr
      }
      currentTag = None

    def addPendingBlankLines(): Unit =
      // Add accumulated blank lines to exposition content
      for _ <- 0 until pendingBlankLines do
        expositionContent += ""
      pendingBlankLines = 0

    for line <- lines do
      val trimmed = line.trim
      // Remove leading * if present, but preserve content after it
      val afterStar =
        if trimmed.startsWith("*") then
          val afterAsterisk = trimmed.drop(1)
          if afterAsterisk.startsWith(" ") then afterAsterisk.drop(1)
          else afterAsterisk
        else trimmed

      val content = afterStar.trim
      val isBlank = content.isEmpty
      val isTag = content.startsWith("@")

      if isTag then
        flushCurrentTag()
        if isSignatureTag(content) then
          if !inSignatureSection then
            inSignatureSection = true
            hasBlankBeforeSignature = pendingBlankLines > 0
          pendingBlankLines = 0  // Discard blank lines before signature section
          currentTagIsSignature = true
          currentTag = Some(new collection.mutable.StringBuilder(content))
        else
          // Exposition tag (@note, @see, @example, or unknown) - preserve blank lines before it
          if foundFirstContent then
            addPendingBlankLines()
          currentTagIsSignature = false
          currentTag = Some(new collection.mutable.StringBuilder(content))
          foundFirstContent = true
      else if isBlank then
        if currentTag.isDefined then
          if !currentTagIsSignature then
            // Blank line after exposition tag - flush tag and track as separator
            flushCurrentTag()
            pendingBlankLines += 1
          // Blank lines in/after signature tags are ignored
        else if !inSignatureSection then
          // Blank line between exposition items - track it
          if foundFirstContent then
            pendingBlankLines += 1
        // Blank lines in signature section are ignored
      else
        // Content line
        if currentTag.isDefined then
          // Continuation of current tag
          currentTag.foreach(_.append("\n").append(afterStar))
        else if !inSignatureSection then
          if !foundFirstContent then
            initialText = Some(content)
            foundFirstContent = true
          else
            // Regular text content - add pending blank lines first
            addPendingBlankLines()
            expositionContent += content

    flushCurrentTag()
    ParsedScaladoc(initialText, expositionContent.toList, signatureTags.toList, hasBlankBeforeSignature)

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

    // Build new signature tags to insert
    val newSignatureTags = collection.mutable.ListBuffer[String]()
    for tp <- missingTparams do
      newSignatureTags += s"@tparam $tp TODO FILL IN"
    for p <- missingParams do
      newSignatureTags += s"@param $p TODO FILL IN"
    if needsReturn then
      newSignatureTags += "@return TODO FILL IN"

    // Check if original was single-line
    val originalBlock = text.substring(block.startIndex, block.endIndex)
    val wasSingleLine = !originalBlock.contains('\n')

    // Combine and sort signature tags in proper order: @tparam, @param, @return
    val allSignatureTags = sortSignatureTags(parsed.signatureTags ++ newSignatureTags.toList)

    buildFormattedBlock(leadingWs, parsed, allSignatureTags, wasSingleLine)

  /** Sort signature tags in proper order: @tparam, then @param, then @return. */
  private def sortSignatureTags(tags: List[String]): List[String] =
    def tagOrder(tag: String): Int =
      if tag.startsWith("@tparam") then 0
      else if tag.startsWith("@param") then 1
      else if tag.startsWith("@return") then 2
      else 3 // Unknown tags go last

    tags.sortBy(tagOrder)

  /** Build a properly formatted Scaladoc block.
   *
   *  Format rules:
   *  1. Initial text appears on same line as opening
   *  2. Continuation lines have * aligned under first * of opening
   *  3. Text after * starts with two spaces
   *  4. Exposition section: text + @note/@see/@example tags
   *  5. Blank line before signature section
   *  6. Signature section: @tparam, @param, @return (no blank lines between)
   */
  private def buildFormattedBlock(
      leadingWs: String,
      parsed: ParsedScaladoc,
      signatureTags: List[String],
      wasSingleLine: Boolean
  ): String =
    val hasExposition = parsed.initialText.isDefined || parsed.expositionContent.nonEmpty
    val hasSignature = signatureTags.nonEmpty

    // Special case: single-line with no additional content needed
    if wasSingleLine && parsed.expositionContent.isEmpty && signatureTags.isEmpty then
      parsed.initialText match
        case Some(text) => s"/** $text */"
        case None => "/** */"

    // Multi-line output needed
    else
      buildMultiLineOutput(leadingWs, parsed, signatureTags)

  private def buildMultiLineOutput(
      leadingWs: String,
      parsed: ParsedScaladoc,
      signatureTags: List[String]
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

    // Exposition content (description lines + exposition tags like @note, @see, @example)
    for line <- parsed.expositionContent do
      if line.isEmpty then
        parts += s"$leadingWs *"
      else if line.startsWith("@") then
        // Exposition tag - may be multi-line, strip trailing empty lines
        val tagLines = line.split("\n", -1).reverse.dropWhile(_.isEmpty).reverse
        for tagLine <- tagLines do
          if tagLine.isEmpty then
            parts += s"$leadingWs *"
          else
            parts += s"$leadingWs *  $tagLine"
      else
        parts += s"$leadingWs *  $line"

    // Blank line before signature section (if there are signature tags)
    if signatureTags.nonEmpty then
      // Check if we need to add a blank line
      val lastLineIsBlank = parsed.expositionContent.lastOption.exists(_.isEmpty) ||
        (parsed.expositionContent.isEmpty && parsed.initialText.isEmpty)

      if !lastLineIsBlank then
        parts += s"$leadingWs *"

    // Signature tags (@tparam, @param, @return)
    // Tags may contain newlines for multi-line content
    for tag <- signatureTags do
      val tagLines = tag.split("\n", -1) // -1 to keep trailing empty strings
      for (line, idx) <- tagLines.zipWithIndex do
        if line.isEmpty then
          parts += s"$leadingWs *"
        else
          parts += s"$leadingWs *  $line"

    // Closing
    parts += s"$leadingWs */"

    parts.mkString("\n")
