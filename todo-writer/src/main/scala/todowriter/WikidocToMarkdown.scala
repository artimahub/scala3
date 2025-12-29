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
 *  - Bold+italic: '''''text''''' -> ***text***
 *  - Bold: '''bold''' -> **bold**
 *  - Italic: ''italic'' -> *italic*
 *  - Headings:
 *    - = Title = -> # Title
 *    - == Section == -> ## Section
 *    - === Subsection === -> ### Subsection
 *  - Do not perform link/bold/italic transformations inside code fences.
 *
 *  This is intentionally small and focused for unit tests; it can be extended
 *  for additional Wikidoc constructs as needed.
 */
object WikidocToMarkdown:

  private val WikiLink: Regex = """\[\[([^\]]+)\]\]""".r
  // Use (?s) flag to make . match newlines for multi-line bold+italic
  private val BoldItalic: Regex = """(?s)'''''(.*?)'''''""".r
  private val Bold: Regex = """'''(.*?)'''""".r
  private val Italic: Regex = """''(.*?)''""".r
  // Wikidoc heading patterns - order matters (=== before == before =)
  private val Heading3: Regex = """^(\s*)===\s*(.+?)\s*===\s*$""".r
  private val Heading2: Regex = """^(\s*)==\s*(.+?)\s*==\s*$""".r
  private val Heading1: Regex = """^(\s*)=\s*(.+?)\s*=\s*$""".r

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
    import java.util.regex.Matcher

    // First pass: apply multi-line bold+italic outside of code blocks
    val preprocessed = applyOutsideCodeBlocks(inner, content =>
      BoldItalic.replaceAllIn(content, m => Matcher.quoteReplacement(s"***${m.group(1)}***"))
    )

    // Second pass: line-by-line processing
    val lines = preprocessed.linesIterator.toList
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
        // heading conversions (check more specific patterns first)
        l = l match
          case Heading3(indent, text) => s"$indent### $text"
          case Heading2(indent, text) => s"$indent## $text"
          case Heading1(indent, text) => s"$indent# $text"
          case _ => l
        // wikilinks conversion
        l = WikiLink.replaceAllIn(l, m => Matcher.quoteReplacement(convertWikiLink(m.group(1))))
        // bold and italic (single-line only at this point)
        l = Bold.replaceAllIn(l, m => Matcher.quoteReplacement(s"**${m.group(1)}**"))
        l = Italic.replaceAllIn(l, m => Matcher.quoteReplacement(s"*${m.group(1)}*"))
        out += l

    out.mkString("\n")

  /** Apply a transformation function only to content outside {{{ ... }}} code blocks. */
  private def applyOutsideCodeBlocks(content: String, transform: String => String): String =
    val codeBlockPattern = """(?s)\{\{\{.*?\}\}\}""".r
    val codeBlocks = collection.mutable.ArrayBuffer[(Int, Int, String)]()

    // Find all code blocks and their positions
    for m <- codeBlockPattern.findAllMatchIn(content) do
      codeBlocks += ((m.start, m.end, m.matched))

    if codeBlocks.isEmpty then
      transform(content)
    else
      // Build result by processing segments between code blocks
      val sb = new StringBuilder
      var pos = 0
      for (start, end, block) <- codeBlocks do
        if pos < start then
          sb.append(transform(content.substring(pos, start)))
        sb.append(block)
        pos = end
      if pos < content.length then
        sb.append(transform(content.substring(pos)))
      sb.toString