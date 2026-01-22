package todowriter

import scala.util.matching.Regex

/** Simple migrator from Wikidoc-style scaladoc fragments to Markdown.
 *
 *  Rules implemented:
 *  - Triple-brace code blocks {{{ ... }}} -> fenced code blocks ``` ... ```
 *  - Wikilinks:
 *    - [[https://url display text]] -> [display text](https://url) (only URLs are converted)
 *    - All other forms (e.g., [[Name]], [[scala.Double the double type]]) -> left unchanged
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
   *  - URL with display text: [[https://url text]] -> [text](https://url)
   *  - All other forms are left unchanged as [[content]] (Scala2 wikidoc style)
   *
   *  Note: Links to code artifacts like [[scala.Double the double type]] are NOT
   *  converted to markdown links because they are not valid URLs. These are
   *  Scala2 wikidoc-style links that documentation tools handle specially.
   */
  private def convertWikiLink(content: String): String =
    val spaceIdx = content.indexOf(' ')
    if spaceIdx > 0 && (content.startsWith("http://") || content.startsWith("https://")) then
      // URL link: [[https://example.com/path Display Text]]
      val url = content.substring(0, spaceIdx)
      val displayText = content.substring(spaceIdx + 1)
      s"[$displayText]($url)"
    else
      // All other forms: leave unchanged as wikidoc-style link
      // This includes simple refs like [[scala.Double]] and
      // refs with display text like [[scala.Double the double type]]
      s"[[$content]]"

  def migrate(inner: String): String =
    import java.util.regex.Matcher

    // First pass: apply multi-line bold+italic outside of code blocks.
    // Protect both Wikidoc triple-brace blocks ({{{ }}}) and fenced Markdown blocks (``` ```).
    val preprocessed = applyOutsideCodeBlocks(inner, content =>
      BoldItalic.replaceAllIn(content, m => Matcher.quoteReplacement(s"***${m.group(1)}***"))
    )

    // Second pass: line-by-line processing
    val lines = preprocessed.split("\n", -1).toList
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
        // Convert {{{ to ``` when it appears inline (e.g., @example {{{)
        if l.contains("{{{") then
          l = l.replace("{{{", "```")
        if l.contains("}}}") then
          l = l.replace("}}}", "```")
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

      val cleanedLines = origLines.map { line =>
        // Only strip a leading '*' when the first non-whitespace character is '*'.
        // This avoids removing content from lines like "/** ..." where '*' appears as part of the comment start.
        if line.trim.startsWith("*") then
          val starIdx = line.indexOf('*')
          line.substring(starIdx + 1)
        else
          line
      }

      // prefixes (leading whitespace) for original tail/star lines (used to reinsert exact indentation)
      val tailPrefixes: List[String] = origLines.drop(1).map(_.takeWhile(_.isWhitespace))
      val origFirstLeading = origLines.headOption.map(_.takeWhile(_.isWhitespace)).getOrElse("")
      val cleaned = cleanedLines.mkString("\n")
      val origEndsWithEmpty = origLines.lastOption.exists(_.trim.isEmpty)

      // Normalize triple-brace markers to fenced backticks for migrate(), preserving leading whitespace.
      val cleanedArr = cleaned.split("\n", -1).toArray
      val normalizedForMigrate = cleanedArr.map { ln =>
        val t = ln.trim
        if t == "{{{" || t == "}}}" then
          val lead = ln.takeWhile(_.isWhitespace)
          s"$lead```"
        else if t.endsWith("{{{") then
          // Handle cases like "@example {{{"
          val lead = ln.takeWhile(_.isWhitespace)
          val content = ln.trim.dropRight(3).trim
          s"$lead$content ```"
        else if t.endsWith("}}}") then
          // Handle cases like "}}}"
          val lead = ln.takeWhile(_.isWhitespace)
          val content = ln.trim.dropRight(3).trim
          s"$lead$content ```"
        else ln
      }.mkString("\n")

      val migratedNorm = migrate(normalizedForMigrate)

      // Keep migrated output as-is (do not restore {{{ }}}) so the migration is stable/idempotent.
      val migratedRestored = migratedNorm

      // Re-apply scaladoc '*' markers without adding new star-only blank lines.
      val migLines = migratedRestored.split("\n", -1).toList
      val out = new StringBuilder

      // Determine the standard scaladoc prefix (whitespace before '*') from lines that have '*'.
      // This is used for lines that originally didn't have '*' (e.g., lines inside {{{ }}} blocks).
      val standardPrefix = origLines.find(l => l.contains('*') && l.trim.startsWith("*"))
        .map(_.takeWhile(_.isWhitespace))
        .getOrElse(" ")

      if startsWithNewline then
        // Build lines into a buffer and join.
        // Use original per-line prefixes (when available) to preserve indentation before '*'.
        val outLines = collection.mutable.ListBuffer[String]()
        outLines += "" // preserve leading newline when joined

        for idx <- migLines.indices do
          val line = migLines(idx)
          // When the scaladoc inner starts with a newline, migLines[idx] corresponds to cleanedLines[idx].
          // The leading newline is preserved as an empty string at index 0 in both lists.
          val origHadStar = origLines.lift(idx).exists(_.contains('*'))
          val origWasEmpty = cleanedLines.lift(idx).exists(_.trim.isEmpty)
          val lineIsBlank = line.trim.isEmpty

          // Use standard prefix for lines that originally didn't have '*', otherwise use the original prefix.
          val prefix = if origHadStar then
            tailPrefixes.lift(idx).orElse(origLines.lift(idx).map(_.takeWhile(_.isWhitespace))).getOrElse(" ")
          else
            standardPrefix

          if lineIsBlank then
            // Preserve blank lines that were in the original, but don't add new ones
            if origWasEmpty && origHadStar then
              outLines += prefix + "*"
            else if idx == migLines.length - 1 && !origHadStar then
              // Include trailing whitespace (without a '*') at the end
              outLines += line
            else
              () // skip this blank line
          else
            // line already may start with whitespace (it is the cleaned content after the '*')
            // Preserve the content after '*' exactly (do not reintroduce trailing-only spaces).
            outLines += prefix + "*" + line
        out.append(outLines.mkString("\n"))
      else
        if migLines.nonEmpty then
          // Preserve any leading whitespace present in the original first line
          val origFirstLeading = origLines.headOption.map(_.takeWhile(_.isWhitespace)).getOrElse("")
          val firstLine =
            if migLines.head.startsWith(" ") || migLines.head.startsWith("\t") then
              migLines.head
            else
              origFirstLeading + migLines.head
          out.append(firstLine)
        if migLines.length > 1 then
          val tail = migLines.tail
          val outLines = collection.mutable.ListBuffer[String]()
          for idx <- tail.indices do
            val line = tail(idx)
            val origWasEmpty = cleanedLines.lift(idx + 1).exists(_.trim.isEmpty)
            val origHadStar = origLines.lift(idx + 1).exists(_.contains('*'))
            val lineIsBlank = line.trim.isEmpty
            // Use standard prefix for lines that originally didn't have '*', otherwise use the original prefix.
            val prefix = if origHadStar then
              tailPrefixes.lift(idx).getOrElse(origFirstLeading)
            else
              standardPrefix
            if lineIsBlank then
              // Preserve blank lines that were in the original, but don't add new ones
              if origWasEmpty && origHadStar then
                outLines += prefix + "*"
              else
                () // skip this blank line
            else
              outLines += prefix + "*" + line
          out.append("\n").append(outLines.mkString("\n"))

      out.toString
    }