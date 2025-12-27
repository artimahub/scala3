package todowriter

import scala.util.matching.Regex

/** Simple migrator from Wikidoc-style scaladoc fragments to Markdown.
 *
 *  Rules implemented:
 *  - Triple-brace code blocks {{{ ... }}} -> fenced code blocks ``` ... ```
 *  - Wikilinks:
 *    - [[Name]] (no spaces) -> left unchanged as [[Name]]
 *    - [[target display text]] -> [display text](target)
 *    - [[https://url display text]] -> [display text](https://url)
 *  - Bold/italic: '''bold''' -> **bold**, ''italic'' -> *italic*
 *  - Do not perform link/bold/italic transformations inside code fences.
 *
 *  This is intentionally small and focused for unit tests; it can be extended
 *  for additional Wikidoc constructs as needed.
 */
object WikidocToMarkdown:

  private val WikiLink: Regex = """\[\[([^\]]+)\]\]""".r
  private val Bold: Regex = """'''(.*?)'''""".r
  private val Italic: Regex = """''(.*?)''""".r

  /** Convert a wikilink's inner content to the appropriate format.
   *
   *  - No spaces: leave as [[content]] (simple reference)
   *  - URL with display text: [[https://url text]] -> [text](https://url)
   *  - Reference with display text: [[target text]] -> [text](target)
   */
  private def convertWikiLink(content: String): String =
    val spaceIdx = content.indexOf(' ')
    if spaceIdx < 0 then
      // No space - simple reference like [[scala.Double]], leave unchanged
      s"[[$content]]"
    else if content.startsWith("http://") || content.startsWith("https://") then
      // URL link: [[https://example.com/path Display Text]]
      val url = content.substring(0, spaceIdx)
      val displayText = content.substring(spaceIdx + 1)
      s"[$displayText]($url)"
    else
      // Reference with display text: [[scala.Double the double type]]
      val target = content.substring(0, spaceIdx)
      val displayText = content.substring(spaceIdx + 1)
      s"[$displayText]($target)"

  def migrate(inner: String): String =
    val lines = inner.linesIterator.toList
    val out = collection.mutable.ListBuffer[String]()
    var inCode = false

    for raw <- lines do
      val trimmed = raw.trim
      if trimmed == "{{{" then
        inCode = true
        out += "```"
      else if trimmed == "}}}" then
        inCode = false
        out += "```"
      else if inCode then
        // preserve code block content verbatim
        out += raw
      else
        // apply inline conversions only outside code fences
        var l = raw
        // wikilinks conversion
        import java.util.regex.Matcher
        l = WikiLink.replaceAllIn(l, m => Matcher.quoteReplacement(convertWikiLink(m.group(1))))
        // bold then italic
        l = Bold.replaceAllIn(l, m => Matcher.quoteReplacement(s"**${m.group(1)}**"))
        l = Italic.replaceAllIn(l, m => Matcher.quoteReplacement(s"*${m.group(1)}*"))
        out += l

    out.mkString("\n")