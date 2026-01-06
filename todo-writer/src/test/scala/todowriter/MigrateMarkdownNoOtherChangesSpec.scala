package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MigrateMarkdownNoOtherChangesSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "not alter unrelated content or indentation (only migrate wikidoc to markdown)" in {
    val inner =
      """
       *  /**: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
       *   {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
       *  End
       """.stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    def leadingSpacesAfterStar(s: String): Int =
      val idx = s.indexOf('*')
      if idx < 0 then -1
      else
        val after = s.substring(idx + 1)
        after.takeWhile(_.isWhitespace).length

    val origMarkerLine = inner.linesIterator.find(_.contains("{{{")).getOrElse(fail("orig marker missing"))
    val migMarkerLine  = migrated.linesIterator.find(l => l.contains("{{{") || l.contains("```")).getOrElse(fail("migrated marker missing"))

    // indentation after '*' should be preserved exactly
    leadingSpacesAfterStar(origMarkerLine) should be (leadingSpacesAfterStar(migMarkerLine))

    // unrelated inline text must remain present (no extra spaces inserted/removed)
    migrated should include ("/**: ... *: An * At` and `B1 *: ... *: Bn *: Bt")
  }