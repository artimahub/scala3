package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.util.regex.Pattern
import java.util.regex.Matcher

class MigrateMarkdownAppScalaSpec extends AnyFlatSpec with Matchers:

  "migration of App.scala comment" should "preserve the opening line and not remove it" in {
    val source =
      """|  /** The main method.
         |   *  This stores all arguments so that they can be retrieved with `args`
         |   *  and then executes all initialization code segments in the order in which
         |   *  they were passed to `delayedInit`.
         |   *  @param args the arguments passed to the main method
         |   */
         |  final def main(args: Array[String]) = {
         |    this._args = args
         |    for (proc <- initCode) proc()
         |    if (util.Properties.propIsSet("scala.time")) {
         |      val total = currentTime - executionStart
         |      Console.println("[total " + total + "ms]")
         |    }
         |  }
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

    // The opener must remain unchanged
    migratedSource should include ("  /** The main method.")
    // The opener must not be removed or replaced by following lines
    migratedSource should include ("   *  This stores all arguments")
    // No extra star-only line immediately before closing
    migratedSource should not include ("\n   *\n   */")
  }