package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownTupleRegressionSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "not rewrite unrelated inline sequences such as Tuple signature fragments" in {
    val inner =
      """
       *  /**: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
       *   {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
       *  End
       """.stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // The specific inline fragment must be preserved exactly (migration must not rewrite it).
    migrated should include ("/**: ... *: An * At` and `B1 *: ... *: Bn *: Bt")
  }