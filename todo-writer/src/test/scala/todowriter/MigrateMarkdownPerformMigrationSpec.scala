package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.util.regex.Pattern
import java.util.regex.Matcher

class MigrateMarkdownPerformMigrationSpec extends AnyFlatSpec with Matchers:

  "performMigration-like replacement" should "preserve the opener line and not inject extra star before closing" in {
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

    // opener line preserved
    migratedSource should include ("  /** The main method.")
    // no extra star-only line inserted immediately before closing
    migratedSource should not include ("\n *\n  */")
    // closing marker remains adjacent to following code line
    migratedSource should include ("*/ \n  final def main(args: Array[String]) = {")
  }