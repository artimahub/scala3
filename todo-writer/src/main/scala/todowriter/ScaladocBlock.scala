package todowriter

import scala.util.matching.Regex

/** Represents a parsed Scaladoc comment block.
 *
 *  When `synthetic` is true the block represents a declaration that has no
 *  Scaladoc at all. In that case `startIndex == endIndex` is the byte offset
 *  of the first character of the declaration line, and `content` is empty.
 *  The Fixer will insert a brand-new Scaladoc stub at that position.
 */
case class ScaladocBlock(
    content: String,
    startIndex: Int,
    endIndex: Int,
    lineNumber: Int,
    params: List[String],
    tparams: List[String],
    hasReturn: Boolean,
    hasThrows: Boolean,
    synthetic: Boolean = false
)

object ScaladocBlock:
  /** Regex to match Scaladoc blocks /** ... */ */
  val ScaladocPattern: Regex = """(?s)/\*\*(?<inner>.*?)\*/""".r

  /** Regex to extract @param names, allowing Scala-style modifiers before the actual name.
   *  Example: @param private[immutable] val len1 description
   */
  private val ParamTagPattern: Regex =
    """^@param\s+(?:(?:(?:private|protected)(?:\[[^\]]+\])?|val|var|using|implicit|inline|erased|lazy|final|override|open|transparent)\s+)*(`[^`]+`|[A-Za-z_][A-Za-z0-9_]*)""".r

  /** Regex to extract @tparam name. */
  private val TparamTagPattern: Regex =
    """^@tparam\s+(`[^`]+`|[A-Za-z_][A-Za-z0-9_]*)""".r

  /** Regex to detect a real @return tag line. */
  private val ReturnTagPattern: Regex =
    """^@return(?:\s|$)""".r

  /** Regex to detect a real @throws tag line. Matches `@throws X`, `@throws[X] desc`, or `@throws` alone. */
  private val ThrowsTagPattern: Regex =
    """^@throws(?:\s|\[|$)""".r

  /** Find all Scaladoc blocks in the given text. */
  def findAll(text: String): List[ScaladocBlock] =
    ScaladocPattern.findAllMatchIn(text).flatMap { m =>
      val inner = m.group("inner")
      val startIndex = m.start
      val endIndex = m.end

      // Skip matches that occur inside strings or comments.
      if isInsideStringOrComment(text, startIndex) then
        None
      else
        val lineNumber = text.substring(0, startIndex).count(_ == '\n') + 1
        val tags = extractTags(inner)
        Some(ScaladocBlock(
          content = inner,
          startIndex = startIndex,
          endIndex = endIndex,
          lineNumber = lineNumber,
          params = tags.params,
          tparams = tags.tparams,
          hasReturn = tags.hasReturn,
          hasThrows = tags.hasThrows
        ))
    }.toList

  private def isInsideStringOrComment(text: String, index: Int): Boolean =
    var i = 0
    var inLineComment = false
    var inBlockComment = false
    var inString = false
    var inTripleString = false
    var inChar = false
    var escaped = false

    while i < index do
      val ch = text.charAt(i)
      if inLineComment then
        if ch == '\n' then inLineComment = false
      else if inString then
        if escaped then escaped = false
        else if ch == '\\' then escaped = true
        else if ch == '"' then inString = false
      else if inTripleString then
        if ch == '"' && i + 2 < index && text.charAt(i + 1) == '"' && text.charAt(i + 2) == '"' then
          inTripleString = false
          i += 2
      else if inChar then
        if escaped then escaped = false
        else if ch == '\\' then escaped = true
        else if ch == '\'' then inChar = false
      else if inBlockComment then
        if ch == '*' && i + 1 < index && text.charAt(i + 1) == '/' then
          inBlockComment = false
          i += 1
      else
        if ch == '/' && i + 1 < index && text.charAt(i + 1) == '/' then
          inLineComment = true
          i += 1
        else if ch == '/' && i + 1 < index && text.charAt(i + 1) == '*' then
          inBlockComment = true
          i += 1
        else if ch == '"' && i + 2 < index && text.charAt(i + 1) == '"' && text.charAt(i + 2) == '"' then
          inTripleString = true
          i += 2
        else if ch == '"' then inString = true
        else if ch == '\'' then inChar = true

      i += 1

    inLineComment || inBlockComment || inString || inTripleString || inChar

  private case class ExtractedTags(
      params: List[String],
      tparams: List[String],
      hasReturn: Boolean,
      hasThrows: Boolean
  )

  private def extractTags(inner: String): ExtractedTags =
    val params = collection.mutable.ListBuffer[String]()
    val tparams = collection.mutable.ListBuffer[String]()
    var hasReturn = false
    var hasThrows = false

    // Process line by line to handle multi-line scaladoc
    for line <- inner.linesIterator do
      val trimmed = line.trim
      val content = if trimmed.startsWith("*") then trimmed.drop(1).trim else trimmed

      ParamTagPattern.findFirstMatchIn(content).foreach { m =>
        params += normalizeDocTagName(m.group(1))
      }

      TparamTagPattern.findFirstMatchIn(content).foreach { m =>
        tparams += normalizeDocTagName(m.group(1))
      }

      if ReturnTagPattern.findFirstIn(content).nonEmpty then
        hasReturn = true

      if ThrowsTagPattern.findFirstIn(content).nonEmpty then
        hasThrows = true

    ExtractedTags(params.toList, tparams.toList, hasReturn, hasThrows)

  private def normalizeDocTagName(name: String): String =
    if name.length >= 2 && name.head == '`' && name.last == '`' then
      name.substring(1, name.length - 1)
    else name
