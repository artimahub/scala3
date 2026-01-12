package todowriter

import java.nio.file.{Files, Path}

/** Result of fixing a file. */
case class FixResult(
    path: String,
    blocksFixed: Int,
    newContent: Option[String]
)

object Fixer:
  /** Fix all issues in a file and return the result.
   *
   *  @param insertTodo when true insert TODO tags for missing @param/@tparam/@return.
   *                    when false only adjust scaladoc asterisk alignment.
   */
  def fixFile(path: Path, results: List[CheckResult], insertTodo: Boolean = true): FixResult =
    val text = Files.readString(path)
    val (newText, fixCount) = applyFixes(text, results, insertTodo)
 
    if newText != text then
      FixResult(path.toString, fixCount, Some(newText))
    else
      FixResult(path.toString, 0, None)

  /** Write fixed content to file. */
  def writeFixedFile(path: Path, content: String): Unit =
    Files.writeString(path, content)

  /** Apply fixes to text and return (newText, numberOfBlocksFixed).
   *
   *  insertTodo: when true, insert TODO tags for missing signature tags.
   *              when false, only adjust scaladoc asterisk alignment.
   */
  def applyFixes(text: String, results: List[CheckResult]): (String, Int) =
    applyFixes(text, results, true)
  
  def applyFixes(text: String, results: List[CheckResult], insertTodo: Boolean): (String, Int) =
    // Choose which results to process:
    // - when inserting TODOs, only process results that actually need fixes
    // - when only aligning (skip-todo), process any result that reported issues OR
    //   any multi-line scaladoc that would be changed by formatting (alignment-only).
    val sortedResults =
      if insertTodo then
        results.filter(needsFix).sortBy(_.scaladoc.startIndex)(Ordering[Int].reverse)
      else
        results
          .filter { r =>
            val block = r.scaladoc
            val originalBlockText =
              if block.startIndex >= 0 && block.endIndex <= text.length then text.substring(block.startIndex, block.endIndex)
              else ""
            // Skip single-line blocks when not inserting todos.
            val isSingleLine = !originalBlockText.contains('\n')
            r.issues.nonEmpty || (!isSingleLine && buildFixedBlock(text, block, Nil, Nil, false) != originalBlockText)
          }
          .sortBy(_.scaladoc.startIndex)(Ordering[Int].reverse)
    var currentText = text
    var fixCount = 0
 
    for result <- sortedResults do
      val block = result.scaladoc
      val decl = result.declaration
      val issues = result.issues
     
      // Determine missing tags (used only when insertTodo = true)
      val missingParams = issues.collect { case Issue.MissingParam(names) => names }.flatten
      val missingTparams = issues.collect { case Issue.MissingTparam(names) => names }.flatten
      val needsReturn = issues.contains(Issue.MissingReturn)
 
      // Decide what to insert: either the actual missing tags, or none when only aligning
      val (tparamsToInsert, paramsToInsert, returnToInsert) =
        if insertTodo then (missingTparams, missingParams, needsReturn) else (Nil, Nil, false)

      // Check if block is single-line
      val originalBlockText = currentText.substring(block.startIndex, block.endIndex)
      val isSingleLine = !originalBlockText.contains('\n')
  
      // When insertTodo = false (skip-todo mode), skip single-line scaladocs entirely
      // since there's nothing to fix without inserting TODO tags.
      val shouldSkip = !insertTodo && isSingleLine
  
      // Compute the formatted block up-front so we can decide whether an alignment-only
      // change would alter the content (and thus should be applied in skip-todo mode).
      val newBlockPreview =
        if !shouldSkip then
          buildFixedBlock(
            currentText,
            block,
            tparamsToInsert,
            paramsToInsert,
            returnToInsert,
            forceMultiLine = false
          )
        else originalBlockText
  
      // Only proceed if there is something to change:
      // - insertion of missing tags (when insertTodo = true), or
      // - explicit issues present, or
      // - in skip-todo mode, the formatted block differs from the original (alignment-only).
      val shouldApply =
        !shouldSkip && (
          (tparamsToInsert.nonEmpty || paramsToInsert.nonEmpty || returnToInsert) ||
            (!insertTodo && (issues.nonEmpty || newBlockPreview != originalBlockText))
        )
  
      if shouldApply then
        val newBlock = newBlockPreview
        // If buildFixedBlock included the following declaration line in its returned
        // text, avoid duplicating that declaration when splicing the replacement
        // into the original document by advancing the end index accordingly.
        val replacementEnd =
          if block.endIndex < currentText.length then
            val rest = currentText.substring(block.endIndex)
            val nextLine =
              if rest.startsWith("\n") then rest.drop(1).takeWhile(_ != '\n')
              else rest.takeWhile(_ != '\n')
            val nextLineWithLeading = if rest.startsWith("\n") then "\n" + nextLine else nextLine
  
            val newBlockLastNonEmptyTrim = newBlock.split("\n").reverse.find(_.trim.nonEmpty).map(_.trim)
            if nextLine.nonEmpty && (newBlock.endsWith(nextLineWithLeading) || newBlockLastNonEmptyTrim.contains(nextLine.trim)) then
              block.endIndex + nextLineWithLeading.length
            else block.endIndex
          else block.endIndex
  
        // Replace starting at the physical start of the line (include leading
        // indentation) so the formatted block's indentation is used exactly once.
        val lineStartIdx = currentText.lastIndexOf('\n', block.startIndex - 1)
        val replacementStart = if lineStartIdx < 0 then 0 else lineStartIdx + 1
  
        // Splice the formatted block into the document, but avoid making a no-op edit.
        val candidate = currentText.substring(0, replacementStart) +
          newBlock +
          currentText.substring(replacementEnd)
        if (candidate != currentText) then
          currentText = candidate
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

  /** Regex to match the start of a markdown code fence (``` with optional language). */
  private val CodeFenceStart = """^```\w*\s*$""".r

  /** Regex to match the end of a markdown code fence. */
  private val CodeFenceEnd = """^```\s*$""".r

  /** Regex to match the start of a triple-brace code block (`{{{`). */
  private val TripleBraceStart = """^\{\{\{\s*$""".r

  /** Regex to match the end of a triple-brace code block (`}}}`). */
  private val TripleBraceEnd = """^\}\}\}\s*$""".r

  /** Check if a line starts a markdown code fence. */
  private def isCodeFenceStart(line: String): Boolean =
    CodeFenceStart.matches(line.trim)

  /** Check if a line ends a markdown code fence. */
  private def isCodeFenceEnd(line: String): Boolean =
    CodeFenceEnd.matches(line.trim)

  /** Check if a line starts a triple-brace code block. */
  private def isTripleBraceStart(line: String): Boolean =
    TripleBraceStart.matches(line.trim)

  /** Check if a line ends a triple-brace code block. */
  private def isTripleBraceEnd(line: String): Boolean =
    TripleBraceEnd.matches(line.trim)

  /** Parse Scaladoc inner content into structured parts.
   *
   *  Separates exposition content (text + @note/@see/@example) from
   *  signature documentation (@tparam/@param/@return).
   *  Multi-line tags are preserved by joining continuation lines with newlines.
   *  Content inside code fences (``` ... ```) and {{{ ... }}} is preserved as-is.
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
        //if afterStarRaw.startsWith(" ") then afterStarRaw.drop(1) else afterStarRaw

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

  /** Build a fixed Scaladoc block with missing tags inserted and proper indentation. */
  def buildFixedBlock(
      text: String,
      block: ScaladocBlock,
      missingTparams: List[String],
      missingParams: List[String],
      needsReturn: Boolean,
      forceMultiLine: Boolean = false
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
 
    // Normalization: if the parser produced an initialText but the inner block
    // actually started with a blank physical line, treat that initialText as
    // exposition content (so we keep the comment as multi-line).
    val normalizedParsed =
      if parsed.initialText.isDefined && inner.startsWith("\n") then
        ParsedScaladoc(
          initialText = None,
          expositionContent = parsed.initialText.get :: parsed.expositionContent,
          signatureTags = parsed.signatureTags,
          hasBlankBeforeSignature = parsed.hasBlankBeforeSignature
        )
      else
        parsed
 
    // If the descriptive text was placed on the first '*' line (misplaced initial text),
    // move the first exposition line up to become the initialText on the opening line.
    // Only do this when the very first physical line inside the block contains
    // non-empty content and does NOT start with a '*' (otherwise it's a normal
    // multi-line scaladoc and should remain as-is).
    val adjustedParsed =
      if normalizedParsed.initialText.isEmpty && normalizedParsed.expositionContent.nonEmpty &&
         inner.linesIterator.toList.headOption.exists(l => l.trim.nonEmpty && !l.trim.startsWith("*")) then
        ParsedScaladoc(
          initialText = Some(normalizedParsed.expositionContent.head.trim),
          expositionContent = normalizedParsed.expositionContent.tail,
          signatureTags = normalizedParsed.signatureTags,
          hasBlankBeforeSignature = normalizedParsed.hasBlankBeforeSignature
        )
      else normalizedParsed
 
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
    val wasSingleLineOrig = !originalBlock.contains('\n')
    val wasSingleLine = if forceMultiLine then false else wasSingleLineOrig
 
    // Combine and sort signature tags in proper order: @tparam, @param, @return
    val allSignatureTags = sortSignatureTags(adjustedParsed.signatureTags ++ newSignatureTags.toList)
 
    // When adding signature tags to a block that has exposition but no initialText,
    // promote the first exposition line to the opening line so the description appears
    // after "/** ". This matches expectations in tests that move misplaced initial text.
    val displayParsed =
      if adjustedParsed.initialText.isEmpty && adjustedParsed.expositionContent.nonEmpty && newSignatureTags.nonEmpty then
        ParsedScaladoc(
          initialText = Some(adjustedParsed.expositionContent.head.trim),
          expositionContent = adjustedParsed.expositionContent.tail,
          signatureTags = adjustedParsed.signatureTags,
          hasBlankBeforeSignature = adjustedParsed.hasBlankBeforeSignature
        )
      else adjustedParsed
 
    // Determine the indentation of the following physical line (declaration) if any.
    // Prefer the declaration indentation when it is less than the comment indentation
    // (handles cases where the comment is over-indented relative to the declaration).
    val declLeadingWs =
      if block.endIndex < text.length then
        val rest = text.substring(block.endIndex)
        val nextLine =
          if rest.startsWith("\n") then rest.drop(1).takeWhile(_ != '\n')
          else rest.takeWhile(_ != '\n')
        nextLine.takeWhile(_.isWhitespace)
      else ""
    val effectiveLeadingWs =
      if declLeadingWs.nonEmpty && declLeadingWs.length < leadingWs.length then declLeadingWs
      else leadingWs
  
    // Build formatted block using the effective indentation and the possibly adjusted parsed content
    // Determine if there's a following declaration we'll include (so formatting can align to it)
    val hasFollowingDecl =
      if block.endIndex < text.length then
        val rest = text.substring(block.endIndex)
        val nextLine =
          if rest.startsWith("\n") then rest.drop(1).takeWhile(_ != '\n')
          else rest.takeWhile(_ != '\n')
        effectiveLeadingWs.nonEmpty && nextLine.startsWith(effectiveLeadingWs)
      else false
    val formatted = buildFormattedBlock(effectiveLeadingWs, displayParsed, allSignatureTags, wasSingleLine, hasFollowingDecl)
  
    // Return only the formatted scaladoc block. The caller is responsible for
    // splicing it into the document; this avoids modifying or duplicating the
    // following declaration's indentation.
    formatted

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
      wasSingleLine: Boolean,
      hasFollowingDecl: Boolean
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
      buildMultiLineOutput(leadingWs, parsed, signatureTags, hasFollowingDecl, wasSingleLine)

  /** Ensure at least 2 leading spaces while preserving relative indentation.
   *
   *  This function ensures there's at least 2 leading spaces, but preserves
   *  the number of leading spaces (for relative indentation).
   */
  private def ensureMinimumSpacing(line: String): String =
    if line.isEmpty then line
    else
      val leadingSpaces = line.takeWhile(_.isWhitespace).length
      //if leadingSpaces == 2 then line
      if leadingSpaces >= 2 then (" " * leadingSpaces) + line.dropWhile(_.isWhitespace)  // Already has 2+ spaces, preserve as-is
      else "  " + line.dropWhile(_.isWhitespace)  // Add 2 spaces to ensure minimum

  private def buildMultiLineOutput(
      leadingWs: String,
      parsed: ParsedScaladoc,
      signatureTags: List[String],
      hasFollowingDecl: Boolean,
      wasSingleLine: Boolean
  ): String =
    val parts = collection.mutable.ListBuffer[String]()

    // Compute opening and star prefixes:
    // Opening line is one space less than the method indentation (clamped to 0).
    // Place the '*' lines one column further right than the opening '/' so the '*'
    // aligns under the first '*' of the "/**" line. This yields:
    //   openPrefix = max(methodIndent - 1, 0)
    //   starPrefix = openPrefix + 1
    // Use the original leading whitespace from the source so we do not shift
    // code indentation. Place '*' one column after the leading whitespace so
    // it aligns under the first '*' of the "/**" opening.
    val openPrefix = leadingWs
    val starPrefix = leadingWs + " "

    // Opening line with initial text
    parsed.initialText match
      case Some(text) =>
        parts += s"$openPrefix/** $text"
      case None =>
        // No initial text - handle gracefully
        parts += s"$openPrefix/**"

    // Exposition content (description lines + exposition tags like @note, @see, @example)
    var inTripleBrace = false  // Track if we're inside a triple-brace block
    for line <- parsed.expositionContent do
      if line.isEmpty then
        parts += s"$starPrefix*"
      else if line.startsWith("@") then
        // Exposition tag - may be multi-line, strip trailing empty lines
        val tagLines = line.split("\n", -1).reverse.dropWhile(_.isEmpty).reverse
        for tagLine <- tagLines do
          if tagLine.isEmpty then
            parts += s"$starPrefix*"
          else
            val spacedLine = ensureMinimumSpacing(tagLine)
            parts += s"$starPrefix*$spacedLine"
      else
        // Check for triple-brace markers
        val isTripleBraceStart = line.trim.startsWith("{{{") && !inTripleBrace
        val isTripleBraceEnd = line.trim.startsWith("}}}") && inTripleBrace

        // Update triple-brace state
        if isTripleBraceStart then inTripleBrace = true
        else if isTripleBraceEnd then inTripleBrace = false

        // Don't apply ensureMinimumSpacing to lines inside triple-brace blocks
        val spacedLine = if inTripleBrace then line else ensureMinimumSpacing(line)
        parts += s"$starPrefix*$spacedLine"

    // Blank line before signature section (if there are signature tags)
    if signatureTags.nonEmpty then
      val lastLineIsBlank = parsed.expositionContent.lastOption.exists(_.trim.isEmpty) ||
        (parsed.expositionContent.isEmpty && parsed.initialText.isEmpty)
      if !lastLineIsBlank then
        parts += s"$starPrefix*"

    // Signature tags (@tparam, @param, @return)
    for tag <- signatureTags do
      val tagLines = tag.split("\n", -1) // -1 to keep trailing empty strings
      for (line, idx) <- tagLines.zipWithIndex do
        if line.isEmpty then
          parts += s"$starPrefix*"
        else if line.nonEmpty then
          val spacedLine = ensureMinimumSpacing(line)
          parts += s"$starPrefix*$spacedLine"

    // Closing
    parts += s"$starPrefix*/"

    parts.mkString("\n")
