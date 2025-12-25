package todowriter

import scala.util.matching.Regex

/** Simple migrator from Wikidoc-style scaladoc fragments to Markdown.
 *
 *  Rules implemented:
 *  - Triple-brace code blocks {{{ ... }}} -> fenced code blocks ``` ... ```
 *  - Wikilinks [[Name]] -> [Name](Name)
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
        // wikilinks -> [Label](Label)
        l = WikiLink.replaceAllIn(l, m => s"[${m.group(1)}](${m.group(1)})")
        // bold then italic
        l = Bold.replaceAllIn(l, m => s"**${m.group(1)}**")
        l = Italic.replaceAllIn(l, m => s"*${m.group(1)}*")
        out += l

    out.mkString("\n")