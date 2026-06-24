package todowriter

import java.nio.file.{Files, Path}
import java.util.regex.Pattern

/** Converts block comments (/* ... */) to Scaladoc comments (/** ... */) when they appear
 *  immediately before type declarations (class, trait, type) or member definitions (def, val, var).
 */
object CommentToScaladoc:
  /** Convert block comments to scaladoc in a file.
   *
   *  Returns Some(newContent) if changes were made, None otherwise.
   */
  def convertFile(path: Path): Option[String] =
    val text = Files.readString(path)
    val converted = convertText(text)
    if converted != text then Some(converted) else None

  /** Convert block comments to scaladoc in text.
   *
   *  Scans for block comments (/* ... */) and checks if they are immediately
   *  followed (with optional whitespace) by type declarations (class, trait, type)
   *  or member definitions (def, val, var). If so, converts them to scaladoc (/** ... */).
   */
  def convertText(text: String): String =
    // Find the index of the first `package` declaration so we can skip any
    // block comments that appear before it (e.g. license/copyright headers).
    val firstPackageIndex =
      val pkgPattern = Pattern.compile("""(?m)^\s*package\s""")
      val pkgMatcher = pkgPattern.matcher(text)
      if pkgMatcher.find() then pkgMatcher.start() else 0

    // Pattern to match block comments: /* ... */
    // This pattern captures the entire comment including delimiters
    val blockCommentPattern = Pattern.compile("""(?s)/\*(?!\*)(.*?)\*\/""")
    val matcher = blockCommentPattern.matcher(text)
    val sb = new StringBuffer

    while matcher.find() do
      val startIndex = matcher.start()
      val endIndex = matcher.end()

      // Skip comments that appear before the first package declaration (e.g. license headers)
      val isBeforePackage = startIndex < firstPackageIndex

      // Skip inline comments: there must be no non-whitespace text before the /*
      // on the same line, and no non-whitespace text after the */ on the same line.
      val lineStart = text.lastIndexOf('\n', startIndex - 1) + 1
      val textBeforeOnLine = text.substring(lineStart, startIndex)
      val hasCodeBeforeOnLine = textBeforeOnLine.exists(!_.isWhitespace)

      val textAfterOnLine = text.substring(endIndex).takeWhile(c => c != '\n' && c != '\r')
      val hasCodeAfterOnLine = textAfterOnLine.exists(!_.isWhitespace)

      // Get the text immediately after the comment
      val textAfterComment = text.substring(endIndex).dropWhile(_.isWhitespace)

      // Check for indentation on the declaration line
      // Skip the comment, then any newline, then check for spaces/tabs
      val remainingText = text.substring(endIndex)
      val afterNewline = remainingText.dropWhile(c => c != '\n' && c != '\r').dropWhile(c => c == '\n' || c == '\r')
      val hasIndentation = afterNewline.nonEmpty && (afterNewline.head == ' ' || afterNewline.head == '\t')

      if !isBeforePackage && !hasCodeBeforeOnLine && !hasCodeAfterOnLine &&
         !looksLikeCommentedOutCode(matcher.group(1)) && isDeclarationStart(textAfterComment, hasIndentation) then
        // Replace /* with /**
        val comment = matcher.group(0)
        val converted = "/**" + comment.substring(2)
        matcher.appendReplacement(sb, java.util.regex.Matcher.quoteReplacement(converted))
      else
        // Keep the original comment
        matcher.appendReplacement(sb, java.util.regex.Matcher.quoteReplacement(matcher.group(0)))

    matcher.appendTail(sb)
    sb.toString

  /** Returns true if the raw comment inner text (everything between /* and */)
   *  looks like commented-out Scala code rather than natural-language documentation.
   *
   *  Heuristic: if any content line (not prefixed with ' * ') starts with a Scala
   *  declaration keyword (def, val, var, class, trait, object, type), we treat the
   *  whole block as a commented-out code block and leave it alone.
   */
  private def looksLikeCommentedOutCode(inner: String): Boolean =
    val codeLinePattern = Pattern.compile("""^\s*(?:private\s+|protected\s+|final\s+|override\s+|abstract\s+|sealed\s+|case\s+)*(?:def|val|var|class|trait|object|type)\s""")
    inner.linesIterator.exists { line =>
      val stripped = line.stripLeading()
      // Skip lines that are standard comment lines (starting with '*')
      !stripped.startsWith("*") && codeLinePattern.matcher(line).find()
    }

  /** Check if the text starts with a type or member declaration. 
   *  
   *  @param text The text to check (should start with the declaration keyword)
   *  @param hasIndentation Whether the declaration line is indented (indicates it's a member)
   */
  private def isDeclarationStart(text: String, hasIndentation: Boolean): Boolean =
    // Pattern to match declaration keywords at the start (after optional modifiers)
    val declPattern = Pattern.compile(
      """(?:@[\w\(\)\s,."]+\s*)*(?:private|protected|final|override|inline|implicit|given|export|opaque|sealed|abstract|lazy|case\s+)*\s*(class|trait|object|def|val|var|type)\b"""
    )
    val matcher = declPattern.matcher(text)
    if matcher.find() then
      val keyword = matcher.group(1)
      // For val/var, only convert if they appear to be class members (indented)
      if keyword == "val" || keyword == "var" then
        hasIndentation
      else
        // For class, trait, type, object, def - always convert
        true
    else
      false
