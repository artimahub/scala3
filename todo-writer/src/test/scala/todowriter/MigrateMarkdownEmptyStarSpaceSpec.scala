package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownEmptyStarSpaceSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "not produce star-only lines that contain only spaces" in {
    val source =
      """|  /** The command line arguments passed to the application's `main` method.
         |  *   
         |  */
         |  protected final def args: Array[String] = _args
         |""".stripMargin

    val pattern = """(?s)/\*\*(.*?)\*/""".r
    val migrated = pattern.replaceAllIn(source, m => "/**" + WikidocToMarkdown.migrateScaladocInner(m.group(1)) + "*/")

    // failing condition: any line that trims to "*" (i.e., star-only possibly with spaces) must not exist
    migrated.linesIterator.exists(line => line.trim == "*") shouldBe false

    // ensure comment opener and following code remain present
    migrated should include ("/** The command line arguments")
    migrated should include ("*/\n  protected final def args")
  }