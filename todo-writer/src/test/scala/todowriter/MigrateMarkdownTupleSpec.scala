package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownTupleSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "not modify unrelated inline text (keep backticks and asterisks intact)" in {
    val inner =
      """
       *  /**: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
       *   {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
       *  End
       """.stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // Ensure the specific inline content is preserved exactly (migration must not rewrite it).
    migrated should include ("/**: ... *: An * At` and `B1 *: ... *: Bn *: Bt")
  }