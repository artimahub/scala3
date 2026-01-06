package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownPreserveCommentStartSpec extends AnyFlatSpec with Matchers:

  "full file migration" should "preserve a comment's first line and not add an extra '*' before closing */" in {
    val source =
      """|/** The main method.
         | *  This stores all arguments so that they can be retrieved with `args`
         | *  and then executes all initialization code segments in the order in which
         | *  they were passed to `delayedInit`.
         | *  @param args the arguments passed to the main method
         | */
         |final def main(args: Array[String]) = {
         |  this._args = args
         |  for (proc <- initCode) proc()
         |}
         |""".stripMargin

    val Scaladoc = """(?s)/\*\*(.*?)\*/""".r
    val migrated = Scaladoc.replaceAllIn(source, m => "/**" + WikidocToMarkdown.migrateScaladocInner(m.group(1)) + "*/")

    // The original first line after "/**" must be preserved.
    migrated should include ("/** The main method.")

    // The closing comment should remain "*/" on its own line (no extra leading '*' line immediately before it).
    migrated should not include ("\n *\n*/")
    // Ensure the comment close is directly followed by the next code line, preserving spacing.
    migrated should include ("*/\nfinal def main(args: Array[String]) = {")
  }