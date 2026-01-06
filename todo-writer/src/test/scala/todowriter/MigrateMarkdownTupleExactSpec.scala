package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownTupleExactSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "preserve the exact Tuple signature fragment and not remove leading text" in {
    val inner =
      """
       *  /**: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
       *   {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
       *   */
       *  def test(x: Int): Unit = {}
       """.stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // Ensure the specific inline fragment is preserved exactly (no truncation or reordering).
    migrated should include ("/**: ... *: An * At` and `B1 *: ... *: Bn *: Bt")
  }