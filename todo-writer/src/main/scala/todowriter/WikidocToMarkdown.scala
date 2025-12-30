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

    // First pass: apply multi-line bold+italic outside of code blocks.
    // Protect both Wikidoc triple-brace blocks ({{{ }}}) and fenced Markdown blocks (``` ```).
    val preprocessed = applyOutsideCodeBlocks(inner, content =>
      BoldItalic.replaceAllIn(content, m => Matcher.quoteReplacement(s"***${m.group(1)}***"))
    )

    // Second pass: line-by-line processing
    val lines = preprocessed.linesIterator.toList
    val out = collection.mutable.ListBuffer[String]()
    var inCode = false

    for raw <- lines do
      val trimmed = raw.trim
      // Treat both {{{ / }}} and ``` as code fence markers so we preserve their contents.
      val lead = raw.takeWhile(_.isWhitespace)
      if (trimmed == "{{{" || (trimmed.startsWith("```") && !inCode)) then
        inCode = true
        // Always emit fenced backticks for code fence openings so migration is stable.
        out += s"${lead}```"
      else if (trimmed == "}}}" || (trimmed.startsWith("```") && inCode)) then
        inCode = false
        // Always emit fenced backticks for code fence closings so migration is stable.
        out += s"${lead}```"
      else if inCode then
        // preserve code block content verbatim
        out += raw
      else
        // apply inline conversions only outside code fences
        var l = raw
        // wikilinks conversion
        l = WikiLink.replaceAllIn(l, m => Matcher.quoteReplacement(convertWikiLink(m.group(1))))
        // bold and italic (single-line only at this point)
        l = Bold.replaceAllIn(l, m => Matcher.quoteReplacement(s"**${m.group(1)}**"))
        l = Italic.replaceAllIn(l, m => Matcher.quoteReplacement(s"*${m.group(1)}*"))
        out += l

    out.mkString("\n")

  /** Apply a transformation function only to content outside code blocks ({{{ ... }}} or ``` ... ```). */
  private def applyOutsideCodeBlocks(content: String, transform: String => String): String =
    // Match either triple-brace blocks or fenced code blocks (non-greedy).
    val codeBlockPattern = """(?s)(\{\{\{.*?\}\}\}|```.*?```)""".r
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

  /** Migrate the inner content of a Scaladoc comment while preserving the
   *  original leading '*' markers and blank-line placement.
   *
   *  This accepts the raw inner string matched by the Scaladoc regex (which
   *  typically starts with a newline and lines that begin with " * ...").
   *  It strips the leading '*' markers, runs the normal migrate() transformation,
   *  then re-inserts '*' markers so the resulting inner content can be safely
   *  spliced back into the original comment without changing its structure.
   */
  def migrateScaladocInner(inner: String): String =
    // If the scaladoc inner already contains fenced code (```), assume it was
    // migrated previously and return unchanged to ensure idempotency.
    if inner.contains("```") then inner
    else {
      val startsWithNewline = inner.startsWith("\n")
      val origLines = inner.split("\n", -1).toList

      // Strip leading '*' and a single optional space, but normalize marker lines.
      val cleanedLines = origLines.map { line =>
        val starIdx = line.indexOf('*')
        if starIdx >= 0 then
          val after = line.substring(starIdx + 1)
          val afterNoLeading = after.dropWhile(_.isWhitespace)
          // For triple-brace or fenced-code markers preserve the original spacing after the asterisk
          // (do not add or remove spaces) so we can round-trip without changes.
          if afterNoLeading.startsWith("{{{") || afterNoLeading.startsWith("}}}") || afterNoLeading.startsWith("```") then
            after
          else
            if after.startsWith(" ") then after.drop(1) else after
        else line
      }

      val cleaned = cleanedLines.mkString("\n")
   
      // Normalize triple-brace markers to fenced backticks for migrate(), preserving leading whitespace.
      val cleanedArr = cleaned.split("\n", -1).toArray
      val normalizedForMigrate = cleanedArr.map { ln =>
        val t = ln.trim
        if t == "{{{" || t == "}}}" then
          val lead = ln.takeWhile(_.isWhitespace)
          s"$lead```"
        else ln
      }.mkString("\n")

      val migratedNorm = migrate(normalizedForMigrate)

      // Keep migrated output as-is (do not restore {{{ }}}) so the migration is stable/idempotent.
      val migratedRestored = migratedNorm

      // Re-apply scaladoc '*' markers.
      val migLines = migratedRestored.split("\n", -1).toList
      val out = new StringBuilder

      if startsWithNewline then
        out.append("\n")
        var prevWasStarOnly = false
        for idx <- migLines.indices do
          val line = migLines(idx)
          // Avoid inserting a star-only blank line immediately before a triple-brace marker,
          // and avoid emitting consecutive star-only blank lines; both cause oscillation.
          val nextIsMarker =
            if idx + 1 < migLines.length then
              val n = migLines(idx + 1).trim
              n == "{{{" || n == "}}}" || n == "```"
            else false
          if line.isEmpty then
            if !nextIsMarker && !prevWasStarOnly then
              out.append(" *")
              prevWasStarOnly = true
            else
              () // skip unstable/duplicate blank star line
          else
            out.append(" * ").append(line)
            prevWasStarOnly = false
          if idx < migLines.length - 1 then out.append("\n")
      else
        if migLines.nonEmpty then out.append(migLines.head)
        if migLines.length > 1 then
          out.append("\n")
          val tail = migLines.tail
          var prevWasStarOnly = false
          for idx <- tail.indices do
            val line = tail(idx)
            val nextIsMarker =
              if idx + 1 < tail.length then
                val n = tail(idx + 1).trim
                n == "{{{" || n == "}}}" || n == "```"
              else false
            if line.isEmpty then
              if !nextIsMarker && !prevWasStarOnly then
                out.append(" *")
                prevWasStarOnly = true
              else ()
            else
              out.append(" * ").append(line)
              prevWasStarOnly = false
            if idx < tail.length - 1 then out.append("\n")

      out.toString
    }