package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownIndentedStartSpec extends AnyFlatSpec with Matchers:

  "performMigration replacement" should "preserve indentation and the first line after an indented /**" in {
    val source =
      """|  /** The main method.
         |  *  This stores all arguments so that they can be retrieved with `args`
         |  *  and then executes all initialization code segments in the order in which
         |  *  they were passed to `delayedInit`.
         |  *  @param args the arguments passed to the main method
         |  */ 
         |  final def main(args: Array[String]) = {
         |    this._args = args
         |    for (proc <- initCode) proc()
         |  }
         |""".stripMargin

    val pattern = """(?s)/\*\*(.*?)\*/""".r
    val migrated = pattern.replaceAllIn(source, m => "/**" + WikidocToMarkdown.migrateScaladocInner(m.group(1)) + "*/")

    // The indented opening line must be preserved exactly
    migrated should include ("  /** The main method.")
    // The closing comment must not gain an extra star-only line before it
    migrated should not include ("\n *\n  */")
    // The code after the comment must remain on the following line with same indentation
    migrated should include ("*/ \n  final def main(args: Array[String]) = {")
  }