package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownTrailingStarSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "not add a trailing star-only line before the comment close" in {
    val inner =
      """/**
        | *  This stores all arguments so that they can be retrieved with `args`
        | *  and then executes all initialization code segments in the order in which
        | *  they were passed to `delayedInit`.
        | *  @param args the arguments passed to the main method
        | */""".stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // must not end with a star-only line which would insert an extra '*' before the closing */
    //migrated.endsWith("\n *") shouldBe false
    migrated.split("\n").exists(l => l.trim == "*") shouldBe false

    // last content line must be preserved exactly
    migrated should include ("*  @param args the arguments passed to the main method")
  }