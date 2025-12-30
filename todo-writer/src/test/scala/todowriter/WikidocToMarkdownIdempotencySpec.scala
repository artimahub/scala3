package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WikidocToMarkdownIdempotencySpec extends AnyFlatSpec with Matchers:

  "WikidocToMarkdown.migrate" should "be idempotent (running twice yields the same output)" in {
    val in = """Intro
              |
              |{{{
              | [[Link]]
              | '''bold'''
              |}}}
              |
              |See [[scala.Double the double type]] outside code.
              |""".stripMargin

    val first = WikidocToMarkdown.migrate(in)
    val second = WikidocToMarkdown.migrate(first)

    // The second run must not change the output produced by the first run.
    first should be (second)
  }