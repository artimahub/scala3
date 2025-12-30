package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WikidocExceptionIndentSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner" should "be stable for triple-brace example from Exception.scala (idempotent)" in {
    val inner =
      """
       *  {{{
       *    handling(classOf[MalformedURLException], classOf[NullPointerException]) by (_.printStackTrace)
       *  }}}
       *  @group dsl
       """.stripMargin

    val first = WikidocToMarkdown.migrateScaladocInner(inner)
    val second = WikidocToMarkdown.migrateScaladocInner(first)
    // Debug output for failing idempotency case
    println("===== FIRST =====")
    println(first)
    println("===== SECOND =====")
    println(second)
    println("===== DIFF =====")
    println(first.zipAll(second, '\u0000', '\u0000').zipWithIndex.filter{ case ((a,b),_) => a != b }.take(200).map{ case ((a,b),i) => s"$i: '${a}' != '${b}'" }.mkString("\n"))
    first should be (second)
  }