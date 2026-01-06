package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownNoLeadingNewlineSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "preserve inline fragments when inner does NOT start with a newline" in {
    // Simulate the scaladoc inner captured when there's no leading newline (matches the Tuple.scala case).
    val inner =
      """  /**: ... *: An * At` and `B1 *: ... *: Bn *: Bt
        |  where at least one of `At` or `Bt` is `EmptyTuple`,
        |  returns the tuple type `(A1, B1) *: ... *: (An, Bn) *: EmptyTuple`.
        |    """.stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // The specific inline fragment must be preserved exactly.
    migrated should include ("/**: ... *: An * At` and `B1 *: ... *: Bn *: Bt")
  }