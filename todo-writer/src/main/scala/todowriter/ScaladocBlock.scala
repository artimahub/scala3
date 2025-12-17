package todowriter

import scala.util.matching.Regex

/** Represents a parsed Scaladoc comment block. */
case class ScaladocBlock(
    content: String,
    startIndex: Int,
    endIndex: Int,
    lineNumber: Int,
    params: List[String],
    tparams: List[String],
    hasReturn: Boolean,
    isOneLiner: Boolean
)

object ScaladocBlock:
  /** Regex to match Scaladoc blocks /** ... */ */
  val ScaladocPattern: Regex = """(?s)/\*\*(?<inner>.*?)\*/""".r

  /** Regex to extract tags from Scaladoc content.
   *  Matches lines like: * @param name description
   *  or @param name description (without leading *)
   */
  private val TagPattern: Regex = """(?:^\s*\*?\s*)?@(\w+)\s+(\w+)?""".r

  /** Find all Scaladoc blocks in the given text. */
  def findAll(text: String): List[ScaladocBlock] =
    ScaladocPattern.findAllMatchIn(text).flatMap { m =>
      val inner = m.group("inner")
      val startIndex = m.start
      val endIndex = m.end

      // Check if the /** is inside a // line comment
      // Find the start of the line containing the /**
      val lineStart = text.lastIndexOf('\n', startIndex) + 1
      val lineBeforeMatch = text.substring(lineStart, startIndex)

      // If there's a // before the /** on the same line, skip this match
      if lineBeforeMatch.contains("//") then
        None
      else
        val lineNumber = text.substring(0, startIndex).count(_ == '\n') + 1
        val tags = extractTags(inner)
        val oneLiner = isOneLinerContent(inner)
        Some(ScaladocBlock(
          content = inner,
          startIndex = startIndex,
          endIndex = endIndex,
          lineNumber = lineNumber,
          params = tags.params,
          tparams = tags.tparams,
          hasReturn = tags.hasReturn,
          isOneLiner = oneLiner
        ))
    }.toList

  private case class ExtractedTags(
      params: List[String],
      tparams: List[String],
      hasReturn: Boolean
  )

  private def extractTags(inner: String): ExtractedTags =
    val params = collection.mutable.ListBuffer[String]()
    val tparams = collection.mutable.ListBuffer[String]()
    var hasReturn = false

    // Process line by line to handle multi-line scaladoc
    for line <- inner.linesIterator do
      // Look for @tag patterns
      val tagMatches = """@(\w+)(?:\s+(\w+))?""".r.findAllMatchIn(line)
      for m <- tagMatches do
        val tag = m.group(1)
        val name = Option(m.group(2)).getOrElse("")
        tag match
          case "param"  => if name.nonEmpty then params += name
          case "tparam" => if name.nonEmpty then tparams += name
          case "return" => hasReturn = true
          case _        => // ignore other tags

    ExtractedTags(params.toList, tparams.toList, hasReturn)

  /** Determine if the Scaladoc has only a single paragraph of descriptive content.
   *
   *  A "one-liner" (really "one-sentencer") is a Scaladoc where the descriptive
   *  text (ignoring tags) forms a single paragraph without blank line separators.
   *  The text may span multiple physical lines.
   *
   *  Examples of one-liners:
   *  - /** Returns the count. */
   *  - /** Returns a two-dimensional array that contains\n *  the results. */
   *  - /** Gets the value.\n *\n *  @param key the key\n */ (blank before tags is OK)
   *
   *  Examples of NOT one-liners (blank line separates paragraphs):
   *  - /** Computes result.\n *\n *  This is complex.\n */
   */
  private def isOneLinerContent(inner: String): Boolean =
    val lines = inner.linesIterator.toList

    // Track state: have we seen content? have we seen a blank after content?
    var foundContent = false
    var foundBlankAfterContent = false
    var isMultipleParagraphs = false

    val iter = lines.iterator
    while iter.hasNext && !isMultipleParagraphs do
      val line = iter.next()
      val trimmed = line.trim
      val afterStar = if trimmed.startsWith("*") then trimmed.drop(1).trim else trimmed

      val isTag = afterStar.startsWith("@")
      val isBlank = afterStar.isEmpty

      if isTag then
        // Entering tag section - stop checking (done with loop)
        ()
      else if isBlank then
        // Blank line - if we've seen content, mark it
        if foundContent then
          foundBlankAfterContent = true
      else
        // Content line
        if foundBlankAfterContent then
          // We have content after a blank line that came after content
          // This means multiple paragraphs = NOT a one-liner
          isMultipleParagraphs = true
        else
          foundContent = true

    // If we get here, either:
    // - No content at all (only tags) -> treat as one-liner
    // - Content with no blank lines between -> one-liner
    // - Content followed by blank then tags -> one-liner (blank before tags is OK)
    // - Multiple paragraphs -> NOT a one-liner
    !isMultipleParagraphs
