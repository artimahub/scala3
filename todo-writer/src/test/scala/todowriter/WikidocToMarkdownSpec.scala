package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WikidocToMarkdownSpec extends AnyFlatSpec with Matchers:

  "WikidocToMarkdown.migrate" should "convert triple-brace code blocks to fenced code blocks" in {
    val in = """Some text.
              |{{{
              |def x = 1
              |}}}
              |More text.""".stripMargin
    val out = WikidocToMarkdown.migrate(in)
    out should include("```")
    out should include("def x = 1")
    out should include("More text.")
  }

  it should "convert wikilinks to markdown links" in {
    val in = "Refer to [[OtherClass]] for details."
    WikidocToMarkdown.migrate(in) should include("[OtherClass](OtherClass)")
  }

  it should "convert bold and italic markup" in {
    val in = "This is '''bold''' and this is ''italic''."
    WikidocToMarkdown.migrate(in) should include("**bold**")
    WikidocToMarkdown.migrate(in) should include("*italic*")
  }

  it should "not transform inside code blocks" in {
    val in = """Start
              |{{{
              | [[Link]]
              | '''bold'''
              |}}}
              |End""".stripMargin
    val out = WikidocToMarkdown.migrate(in)
    out should include("```")
    out should include(" [[Link]]")
    out should include(" '''bold'''")
    out should include("End")
  }

  it should "handle multiple code blocks and wikilinks in same content" in {
    val in = """Intro [[A]]
              |
              |{{{
              |code
              |}}}
              |
              |Middle [[B]]
              |
              |{{{
              |more code
              |}}}
              |""".stripMargin
    val out = WikidocToMarkdown.migrate(in)
    out should include("[A](A)")
    out should include("[B](B)")
    // Use split with negative limit to preserve trailing empty segments when the
    // content ends with a fence marker.
    out.split("```", -1).length should be (5) // 2 code fences -> 4 markers + surrounding splits
  }