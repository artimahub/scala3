package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.util.regex.Pattern
import java.util.regex.Matcher

class MigrateMarkdownIgnoreCommentedSpec extends AnyFlatSpec with Matchers:

  "performMigration-like replacement" should "not modify scaladoc blocks that are commented out with // prefix" in {
    val source =
      """|  // /** Specifies how the array lengths should vary.
         |  //  *
         |  //  *  By default,  `UnrolledBuffer` uses arrays of a fixed size.  A length
         |  //  *  policy can be given that changes this scheme to, for instance, an
         |  //  *  exponential growth.
         |  //  *
         |  //  *  @param nextLength   computes the length of the next array from the length of the latest one
         |  //  */
         |  // def setLengthPolicy(nextLength: Int => Int): Unit = { myLengthPolicy = nextLength }
         |""".stripMargin

    val pattern = Pattern.compile("(?s)^(?!.*//)/\\*\\*(.*?)\\*/", Pattern.MULTILINE)
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

    val expected =
      """|  // /** Specifies how the array lengths should vary.
         |  //  *
         |  //  *  By default,  `UnrolledBuffer` uses arrays of a fixed size.  A length
         |  //  *  policy can be given that changes this scheme to, for instance, an
         |  //  *  exponential growth.
         |  //  *
         |  //  *  @param nextLength   computes the length of the next array from the length of the latest one
         |  //  */
         |  // def setLengthPolicy(nextLength: Int => Int): Unit = { myLengthPolicy = nextLength }
         |""".stripMargin

    // The commented-out block must remain unchanged.
    migratedSource shouldBe expected
  }