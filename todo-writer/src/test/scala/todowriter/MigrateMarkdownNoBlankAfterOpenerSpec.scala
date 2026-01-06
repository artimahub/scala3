package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.util.regex.Pattern
import java.util.regex.Matcher

class MigrateMarkdownNoBlankAfterOpenerSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "not insert a star-only blank line immediately after opening /**" in {
    val source =
      """|/**
         | * Compares two Boolean expressions and returns `true` if they evaluate to a different value.
         | *
         | * `a != b` returns `true` if and only if
         | *  - `a` is `true` and `b` is `false` or
         | *  - `a` is `false` and `b` is `true`.
         | *
         | */
         |def !=(x: Boolean): Boolean
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

    // must not insert a star-only blank line immediately after the opener
    migratedSource should not include ("\n *\n * Compares")
    // opener must be followed directly by the first content line
    migratedSource should include ("/**\n * Compares")
  }