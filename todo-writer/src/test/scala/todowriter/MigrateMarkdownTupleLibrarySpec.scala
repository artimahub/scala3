package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownTupleLibrarySpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "preserve the exact Tuple.scala inline fragment" in {
    val inner =
      """
       *  /**: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
       *  where at least one of `At` or `Bt` is `EmptyTuple`,
       *  returns the tuple type `(A1, B1) *: ... *: (An, Bn) *: EmptyTuple`.
       *    */
       """.stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // The fragment "/**: ... *: An * At` and `B1 *: ... *: Bn *: Bt" must remain intact.
    migrated should include ("/**: ... *: An * At` and `B1 *: ... *: Bn *: Bt")
  }