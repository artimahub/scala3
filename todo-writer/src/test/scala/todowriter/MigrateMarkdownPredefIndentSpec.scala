package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.util.regex.Pattern
import java.util.regex.Matcher

class MigrateMarkdownPredefIndentSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "preserve alignment for trailing tag lines (group) without adding extra indentation" in {
    val source =
      """|  /**
         |   * A method that returns its input value.
         |   * @tparam A type of the input value x.
         |   * @param x the value of type `A` to be returned.
         |   * @return the value `x`.
         |   * @group utilities 
         |   */
         |""".stripMargin

    val pattern = Pattern.compile("(?s)/\\*\\*(.*?)\\*/")
    val matcher = pattern.matcher(source)
    val sb = new StringBuffer
    var any = false
    while matcher.find() do
      val inner = matcher.group(1)
      val migrated = WikidocToMarkdown.migrateScaladocInner(inner)
      if migrated != inner then
        any = true
        val replacement = "/**" + migrated + "*/"
        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    matcher.appendTail(sb)
    val migratedSource = if any then sb.toString else source

    // Expect no changes to spacing/asterisk alignment for the @group line.
    migratedSource shouldBe source
  }