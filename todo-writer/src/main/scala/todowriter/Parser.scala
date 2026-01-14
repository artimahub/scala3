package todowriter

/** Parsed representation of Scaladoc content.
 *
 *  Scaladoc has two logical sections:
 *  1. Exposition: text paragraphs + exposition tags (@note, @see, @example)
 *  2. Signature: @tparam, @param, @return tags
 */
case class ParsedScaladoc(
    initialText: Option[String],           // First line of description (goes on /** line)
    expositionContent: List[String],       // Additional description lines and exposition tags
    signatureTags: List[String],           // Signature tags (@tparam, @param, @return)
    hasBlankBeforeSignature: Boolean       // Whether there was a blank line before signature tags
)

object Parser:
  /** Tags that are part of the exposition (main content). */
  val ExpositionTags = Set("@note", "@see", "@example")

  /** Tags that document the signature (appear at end). */
  val SignatureTags = Set("@tparam", "@param", "@return")

  /** Check if a line starts with an exposition tag. */
  def isExpositionTag(line: String): Boolean =
    ExpositionTags.exists(tag => line.startsWith(tag))

  /** Check if a line starts with a signature tag. */
  def isSignatureTag(line: String): Boolean =
    SignatureTags.exists(tag => line.startsWith(tag))

  /** Regex to match the start of a markdown code fence (``` with optional language). */
  private val CodeFenceStart = """^```\w*\s*$""".r

  /** Regex to match the end of a markdown code fence. */
  private val CodeFenceEnd = """^```\s*$""".r

  /** Regex to match the start of a triple-brace code block (`{{{`). */
  private val TripleBraceStart = """^\{\{\{\s*$""".r

  /** Regex to match the end of a triple-brace code block (`}}}`). */
  private val TripleBraceEnd = """^\}\}\}\s*$""".r

  /** Check if a line starts a markdown code fence. */
  def isCodeFenceStart(line: String): Boolean =
    CodeFenceStart.matches(line.trim)

  /** Check if a line ends a markdown code fence. */
  def isCodeFenceEnd(line: String): Boolean =
    CodeFenceEnd.matches(line.trim)

  /** Check if a line starts a triple-brace code block. */
  def isTripleBraceStart(line: String): Boolean =
    TripleBraceStart.matches(line.trim)

  /** Check if a line ends a triple-brace code block. */
  def isTripleBraceEnd(line: String): Boolean =
    TripleBraceEnd.matches(line.trim)

  /** Parse Scaladoc inner content into structured parts.
   *
   *  Separates exposition content (text + @note/@see/@example) from
   *  signature documentation (@tparam/@param/@return).
   *  Multi-line tags are preserved by joining continuation lines with newlines.
   *  Content inside code fences (``` ... ```) and {{{ ... }}} is preserved as-is.
   */
  def parseScaladocContent(inner: String): ParsedScaladoc =
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
    var inCodeFence = false  // Track if we're inside a ``` code fence
    var inTripleBrace = false // Track if we're inside a {{{ }}} code block

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
      // Use the original physical line to preserve internal spacing inside code blocks.
      val rawLine = line
      val trimmed = rawLine.trim
      val hadStar = trimmed.startsWith("*")
      // Compute raw content after the first '*' in the physical line so we keep any
      // spaces that followed the asterisk in the original source.
      val afterStarRaw =
        if hadStar then
          val starIdx = rawLine.indexOf('*')
          if starIdx >= 0 then rawLine.substring(starIdx + 1) else rawLine.drop(1)
        else rawLine

      // Normalized form (drop a single leading space if present) for marker detection
      val afterStarNorm = afterStarRaw

      // Detect markers using the normalized form. Compute booleans first so we can
      // preserve raw spacing for the marker lines themselves, then update state.
      val marker = afterStarNorm.trim
      val isStartCodeFence = isCodeFenceStart(marker) && !inCodeFence && !inTripleBrace
      val isEndCodeFence = isCodeFenceEnd(marker) && inCodeFence
      val isStartTriple = isTripleBraceStart(marker) && !inTripleBrace && !inCodeFence
      val isEndTriple = isTripleBraceEnd(marker) && inTripleBrace

      // Preserve raw spacing when inside a triple-brace block or when this line is
      // the triple-brace start/end marker. Otherwise use the normalized form.
      val content =
        if inTripleBrace || isStartTriple || isEndTriple then afterStarRaw
        else afterStarNorm

      // Update fence/triple state after choosing content so marker lines are preserved.
      if isStartCodeFence then
        inCodeFence = true
      else if isEndCodeFence then
        inCodeFence = false
      else if isStartTriple then
        inTripleBrace = true
      else if isEndTriple then
        inTripleBrace = false

      val isBlank = content.trim.isEmpty
      val isTag = (isExpositionTag(content.trim) || isSignatureTag(content.trim)) && !inCodeFence && !inTripleBrace

      if isTag then
        flushCurrentTag()
        if isSignatureTag(content.trim) then
          if !inSignatureSection then
            inSignatureSection = true
            hasBlankBeforeSignature = pendingBlankLines > 0
          pendingBlankLines = 0  // Discard blank lines before signature section
          currentTagIsSignature = true
          currentTag = Some(new collection.mutable.StringBuilder(content.trim))
        else
          // Exposition tag (@note, @see, @example, or unknown) - preserve blank lines before it
          if foundFirstContent then
            addPendingBlankLines()
          currentTagIsSignature = false
          currentTag = Some(new collection.mutable.StringBuilder(content.trim))
          foundFirstContent = true
      else if isBlank && !inCodeFence && !inTripleBrace then
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
        // Content line (or inside code fence/triple-brace)
        if currentTag.isDefined then
          // Continuation of current tag - preserve original spacing for code blocks
          if inTripleBrace then currentTag.foreach(_.append("\n").append(afterStarRaw))
          else currentTag.foreach(_.append("\n").append(content))
        else if !inSignatureSection then
          // If this line had a leading '*' it is exposition content; otherwise it may be
          // the initial text that belonged on the opening line.
          if hadStar then
            if !foundFirstContent then 
              foundFirstContent = true
            else 
              addPendingBlankLines()
            // Preserve raw spacing when inside triple-brace, otherwise use afterStarRaw to preserve relative indentation
            expositionContent += afterStarRaw
          else
            if !foundFirstContent then
              initialText = Some(content.trim)
              foundFirstContent = true
            else if (content.trim.nonEmpty) then
              // Regular text content - add pending blank lines first
              addPendingBlankLines()
              expositionContent += afterStarRaw

    flushCurrentTag()
    ParsedScaladoc(initialText, expositionContent.toList, signatureTags.toList, hasBlankBeforeSignature)