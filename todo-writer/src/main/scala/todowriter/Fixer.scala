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

  /** Represents a single item in the Scaladoc content. */
  private sealed trait DocItem
  private case class TextLine(text: String) extends DocItem
  private case class BlankLine() extends DocItem
  private case class TagItem(text: String, isSignature: Boolean) extends DocItem

  /** Parsed representation of Scaladoc content.
   *
   *  Items are stored in their original order. Signature tags will be sorted
   *  among themselves while non-signature items stay in place.
   */
  private case class ParsedScaladoc(
      initialText: Option[String],  // First line of description (goes on /** line)
      items: List[DocItem]          // All other content items in order
  )

  /** Tags that document the signature (sorted relative to each other). */
  private val SignatureTags = Set("@tparam", "@param", "@return", "@throws")

  /** Check if a line starts with a signature tag. */
  private def isSignatureTag(line: String): Boolean =
    SignatureTags.exists(tag => line.startsWith(tag))

  /** Check if a line starts with any @ tag. */
  private def isAnyTag(line: String): Boolean =
    line.startsWith("@")

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
   *  Items are stored in order. Signature tags are marked so they can be
   *  sorted among themselves while non-signature items stay in place.
   *  Multi-line tags are preserved by joining continuation lines with newlines.
   *  Content inside code fences (``` ... ```) and {{{ ... }}} is preserved as-is.
   */
  private def parseScaladocContent(inner: String): ParsedScaladoc =
    val lines = inner.linesIterator.toList

    var initialText: Option[String] = None
    val items = collection.mutable.ListBuffer[DocItem]()
    var foundFirstContent = false
    var pendingBlankLines = 0  // Track blank lines to add before next content
    var currentTag: Option[collection.mutable.StringBuilder] = None
    var currentTagIsSignature = false
    var inCodeFence = false  // Track if we're inside a ``` code fence
    var inTripleBrace = false // Track if we're inside a {{{ }}} code block

    def flushCurrentTag(): Unit =
      currentTag.foreach { tag =>
        val tagStr = tag.toString
        items += TagItem(tagStr, currentTagIsSignature)
      }
      currentTag = None

    def addPendingBlankLines(): Unit =
      // Add accumulated blank lines to items
      for _ <- 0 until pendingBlankLines do
        items += BlankLine()
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
      val isTag = isAnyTag(content.trim) && !inCodeFence && !inTripleBrace

      if isTag then
        flushCurrentTag()
        // Add pending blank lines before this tag
        if foundFirstContent then
          addPendingBlankLines()
        currentTagIsSignature = isSignatureTag(content.trim)
        currentTag = Some(new collection.mutable.StringBuilder(content.trim))
        foundFirstContent = true
      else if isBlank && !inCodeFence && !inTripleBrace then
        if currentTag.isDefined then
          // Blank line after a tag - flush tag and track as separator
          flushCurrentTag()
          pendingBlankLines += 1
        else if foundFirstContent then
          // Blank line between items - track it
          pendingBlankLines += 1
      else
        // Content line (or inside code fence/triple-brace)
        if currentTag.isDefined then
          // Continuation of current tag - preserve original spacing for code blocks
          if inTripleBrace then currentTag.foreach(_.append("\n").append(afterStarRaw))
          else currentTag.foreach(_.append("\n").append(content))
        else
          // If this line had a leading '*' it is regular content; otherwise it may be
          // the initial text that belonged on the opening line.
          if hadStar then
            if !foundFirstContent then
              foundFirstContent = true
            else
              addPendingBlankLines()
            // Preserve raw spacing when inside triple-brace, otherwise use afterStarRaw to preserve relative indentation
            items += TextLine(afterStarRaw)
          else
            if !foundFirstContent then
              initialText = Some(content.trim)
              foundFirstContent = true
            else if content.trim.nonEmpty then
              // Regular text content - add pending blank lines first
              addPendingBlankLines()
              items += TextLine(afterStarRaw)

    flushCurrentTag()
    ParsedScaladoc(initialText, items.toList)

  /** Build a fixed Scaladoc block with missing tags inserted and proper indentation.
   *
   *  Signature tags (@tparam, @param, @return, @throws) are sorted relative to each other.
   *  All other tags remain in their original positions.
   */
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
    // a text line (so we keep the comment as multi-line).
    val normalizedParsed =
      if parsed.initialText.isDefined && inner.startsWith("\n") then
        ParsedScaladoc(
          initialText = None,
          items = TextLine(parsed.initialText.get) :: parsed.items
        )
      else
        parsed

    // If the descriptive text was placed on the first '*' line (misplaced initial text),
    // move the first text line up to become the initialText on the opening line.
    // Only do this when the very first physical line inside the block contains
    // non-empty content and does NOT start with a '*' (otherwise it's a normal
    // multi-line scaladoc and should remain as-is).
    val adjustedParsed =
      if normalizedParsed.initialText.isEmpty &&
         inner.linesIterator.toList.headOption.exists(l => l.trim.nonEmpty && !l.trim.startsWith("*")) then
        normalizedParsed.items.collectFirst { case TextLine(t) => t } match
          case Some(firstText) =>
            val remainingItems = normalizedParsed.items.dropWhile {
              case TextLine(_) => false
              case _ => true
            }.drop(1)
            // Prepend any items that came before the first TextLine, but strip
            // trailing BlankLines since they were separators between the tag
            // and the text we're promoting (no longer needed).
            val prefixItems = normalizedParsed.items.takeWhile {
              case TextLine(_) => false
              case _ => true
            }.reverse.dropWhile {
              case BlankLine() => true
              case _ => false
            }.reverse
            ParsedScaladoc(
              initialText = Some(firstText.trim),
              items = prefixItems ++ remainingItems
            )
          case None => normalizedParsed
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

    // Process items: sort signature tags among themselves, keep others in place
    val processedItems = processItemsWithSignatureTags(adjustedParsed.items, newSignatureTags.toList)

    // When the block has no initialText, promote the first text line to the
    // opening line so the description appears after "/** ". This ensures
    // consistent formatting regardless of whether we're adding signature tags.
    val (displayInitialText, displayItems) =
      if adjustedParsed.initialText.isEmpty then
        processedItems.collectFirst { case TextLine(t) => t } match
          case Some(firstText) =>
            val itemsBeforeText = processedItems.takeWhile {
              case TextLine(_) => false
              case _ => true
            }
            val itemsAfterText = processedItems.dropWhile {
              case TextLine(_) => false
              case _ => true
            }.drop(1)
            (Some(firstText.trim), itemsBeforeText ++ itemsAfterText)
          case None => (None, processedItems)
      else
        (adjustedParsed.initialText, processedItems)

    // Ensure there's a blank line before signature tags when we have initial text
    // but no other content before the signature tags
    val finalItems =
      if displayInitialText.isDefined && newSignatureTags.nonEmpty then
        // Check if first item is a signature tag (meaning no content between initial text and tags)
        displayItems.headOption match
          case Some(TagItem(_, true)) =>
            // Need to add blank line before signature tags
            BlankLine() :: displayItems
          case _ => displayItems
      else
        displayItems

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

    // Build formatted block
    val formatted = buildFormattedBlock(effectiveLeadingWs, displayInitialText, finalItems, wasSingleLine)

    // Return only the formatted scaladoc block. The caller is responsible for
    // splicing it into the document; this avoids modifying or duplicating the
    // following declaration's indentation.
    formatted

  /** Process items: sort signature tags among themselves and insert new ones.
   *
   *  Algorithm:
   *  1. Extract existing signature tags and their positions
   *  2. Sort all signature tags (existing + new)
   *  3. Replace signature tag positions with sorted tags
   *  4. Insert new signature tags at appropriate positions
   */
  private def processItemsWithSignatureTags(items: List[DocItem], newTags: List[String]): List[DocItem] =
    // Find positions of existing signature tags
    val sigTagPositions = items.zipWithIndex.collect {
      case (TagItem(text, true), idx) => (idx, text)
    }

    if sigTagPositions.isEmpty && newTags.isEmpty then
      // No signature tags to process
      items
    else if sigTagPositions.isEmpty then
      // No existing signature tags - append new ones at the end with a blank line before
      val sortedNewTags = sortSignatureTags(newTags)
      // Add blank line before new signature tags if there's content before them
      val needsBlankLine = items.nonEmpty && !items.lastOption.exists {
        case BlankLine() => true
        case _ => false
      }
      val blankLinePrefix = if needsBlankLine then List(BlankLine()) else Nil
      items ++ blankLinePrefix ++ sortedNewTags.map(t => TagItem(t, true))
    else
      // Extract existing signature tag texts
      val existingSigTags = sigTagPositions.map(_._2)
      // Sort all signature tags together
      val allSortedSigTags = sortSignatureTags(existingSigTags ++ newTags)

      // Build new items list:
      // - Non-signature items stay in place
      // - Signature tag positions get filled with sorted tags
      // - Extra new tags get inserted after the last signature tag position
      val result = collection.mutable.ListBuffer[DocItem]()
      var sortedTagIdx = 0
      var lastSigTagResultIdx = -1

      for (item, idx) <- items.zipWithIndex do
        item match
          case TagItem(_, true) =>
            // This is a signature tag position - fill with next sorted tag
            if sortedTagIdx < allSortedSigTags.length then
              result += TagItem(allSortedSigTags(sortedTagIdx), true)
              lastSigTagResultIdx = result.length - 1
              sortedTagIdx += 1
          case other =>
            result += other

      // Insert any remaining sorted tags after the last signature tag
      while sortedTagIdx < allSortedSigTags.length do
        val insertIdx = if lastSigTagResultIdx >= 0 then lastSigTagResultIdx + 1 else result.length
        result.insert(insertIdx, TagItem(allSortedSigTags(sortedTagIdx), true))
        lastSigTagResultIdx = insertIdx
        sortedTagIdx += 1

      result.toList

  /** Sort signature tags in proper order: @tparam, @param, @return, @throws. */
  private def sortSignatureTags(tags: List[String]): List[String] =
    def tagOrder(tag: String): Int =
      if tag.startsWith("@tparam") then 0
      else if tag.startsWith("@param") then 1
      else if tag.startsWith("@return") then 2
      else if tag.startsWith("@throws") then 3
      else 4 // Unknown signature tags go last

    tags.sortBy(tagOrder)

  /** Build a properly formatted Scaladoc block.
   *
   *  Format rules:
   *  1. Initial text appears on same line as opening
   *  2. Continuation lines have * aligned under first * of opening
   *  3. Text after * starts with two spaces
   *  4. Items appear in their original order (signature tags sorted among themselves)
   */
  private def buildFormattedBlock(
      leadingWs: String,
      initialText: Option[String],
      items: List[DocItem],
      wasSingleLine: Boolean
  ): String =
    // Special case: single-line output when there's only initial text and no other items.
    // This applies when the original was single-line, or when we've promoted all content
    // to the initial text line (for consistent formatting).
    if items.isEmpty && initialText.isDefined then
      s"$leadingWs/** ${initialText.get} */"
    else if wasSingleLine && items.isEmpty then
      "/** */"

    // Multi-line output needed
    else
      buildMultiLineOutput(leadingWs, initialText, items)

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
      initialText: Option[String],
      items: List[DocItem]
  ): String =
    val parts = collection.mutable.ListBuffer[String]()

    // Compute opening and star prefixes:
    // Use the original leading whitespace from the source so we do not shift
    // code indentation. Place '*' one column after the leading whitespace so
    // it aligns under the first '*' of the "/**" opening.
    val openPrefix = leadingWs
    val starPrefix = leadingWs + " "

    // Opening line with initial text
    initialText match
      case Some(text) =>
        parts += s"$openPrefix/** $text"
      case None =>
        // No initial text - handle gracefully
        parts += s"$openPrefix/**"

    // Output all items in order
    var inTripleBrace = false  // Track if we're inside a triple-brace block
    for item <- items do
      item match
        case BlankLine() =>
          parts += s"$starPrefix*"

        case TextLine(line) =>
          if line.isEmpty then
            parts += s"$starPrefix*"
          else
            // Check for triple-brace markers
            val isTripleBraceStart = (line.trim.startsWith("{{{") || line.trim.startsWith("```")) && !inTripleBrace
            val isTripleBraceEnd = (line.trim.startsWith("}}}") || line.trim.startsWith("```")) && inTripleBrace

            // Update triple-brace state
            if isTripleBraceStart then inTripleBrace = true
            else if isTripleBraceEnd then inTripleBrace = false

            // Always apply ensureMinimumSpacing (it preserves existing spacing if >= 2 spaces)
            val spacedLine = ensureMinimumSpacing(line)
            parts += s"$starPrefix*$spacedLine"

        case TagItem(tag, _) =>
          // Tag - may be multi-line, strip trailing empty lines
          val tagLines = tag.split("\n", -1).reverse.dropWhile(_.trim.isEmpty).reverse
          for tagLine <- tagLines do
            if tagLine.isEmpty then
              parts += s"$starPrefix*"
            else
              val spacedLine = ensureMinimumSpacing(tagLine)
              parts += s"$starPrefix*$spacedLine"

    // Closing
    parts += s"$starPrefix*/"

    parts.mkString("\n")
