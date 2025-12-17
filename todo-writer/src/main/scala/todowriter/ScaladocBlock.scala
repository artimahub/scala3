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
  val ScaladocPattern: Regex = """(?s)/\*\*(.*?)\*/""".r("inner")

  /** Regex to extract tags from Scaladoc content.
   *  Matches lines like: * @param name description
   *  or @param name description (without leading *)
   */
  private val TagPattern: Regex = """(?:^\s*\*?\s*)?@(\w+)\s+(\w+)?""".r

  /** Find all Scaladoc blocks in the given text. */
  def findAll(text: String): List[ScaladocBlock] =
    ScaladocPattern.findAllMatchIn(text).map { m =>
      val inner = m.group("inner")
      val startIndex = m.start
      val endIndex = m.end
      val lineNumber = text.substring(0, startIndex).count(_ == '\n') + 1
      val tags = extractTags(inner)
      val oneLiner = isOneLinerContent(inner)
      ScaladocBlock(
        content = inner,
        startIndex = startIndex,
        endIndex = endIndex,
        lineNumber = lineNumber,
        params = tags.params,
        tparams = tags.tparams,
        hasReturn = tags.hasReturn,
        isOneLiner = oneLiner
      )
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

  /** Determine if the Scaladoc has only a single line of descriptive content.
   *
   *  A "one-liner" is a Scaladoc where the descriptive text (ignoring tags like
   *  @param, @tparam, @return) consists of only a single line/sentence.
   *
   *  Examples of one-liners:
   *  - /** Returns the count. */
   *  - /** Gets the value.\n *  @param key the key\n */
   *
   *  Examples of NOT one-liners:
   *  - /** Computes result.\n *  This is complex.\n */
   */
  private def isOneLinerContent(inner: String): Boolean =
    // Extract non-tag content lines
    val contentLines = inner
      .linesIterator
      .map(_.trim)
      .map { line =>
        // Remove leading * if present
        if line.startsWith("*") then line.drop(1).trim else line
      }
      .filter(_.nonEmpty)
      .filterNot(_.startsWith("@")) // Remove tag lines
      .toList

    // It's a one-liner if there's exactly 0 or 1 content lines
    // (0 means only tags, which we treat as one-liner for @return purposes)
    contentLines.size <= 1
