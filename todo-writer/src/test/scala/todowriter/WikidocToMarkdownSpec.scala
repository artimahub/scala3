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

  it should "leave simple wikilinks unchanged" in {
    val in = "Refer to [[OtherClass]] for details."
    WikidocToMarkdown.migrate(in) should include("[[OtherClass]]")
  }

  it should "convert wikilinks with display text to markdown links" in {
    val in = "See [[scala.Double the double type]] for more."
    WikidocToMarkdown.migrate(in) should include("[the double type](scala.Double)")
  }

  it should "convert URL wikilinks to markdown links" in {
    val in = "Read [[https://docs.scala-lang.org/overview Value Classes]] for details."
    WikidocToMarkdown.migrate(in) should include("[Value Classes](https://docs.scala-lang.org/overview)")
  }

  it should "convert bold and italic markup" in {
    val in = "This is '''bold''' and this is ''italic''."
    WikidocToMarkdown.migrate(in) should include("**bold**")
    WikidocToMarkdown.migrate(in) should include("*italic*")
  }

  it should "convert bold+italic (5 quotes) to triple asterisks" in {
    val in = "'''''This is bold and italic.'''''"
    WikidocToMarkdown.migrate(in) should be("***This is bold and italic.***")
  }

  it should "handle bold+italic spanning multiple lines" in {
    val in = """'''''It should be noted that this trait is implemented using the [[DelayedInit]]
              |functionality, which means that fields of the object will not have been initialized
              |before the main method has been executed.'''''""".stripMargin
    val out = WikidocToMarkdown.migrate(in)
    out should startWith("***It should be noted")
    out should endWith("executed.***")
    out should include("[[DelayedInit]]")
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
    // Simple wikilinks without display text are left unchanged
    out should include("[[A]]")
    out should include("[[B]]")
    // Use split with negative limit to preserve trailing empty segments when the
    // content ends with a fence marker.
    out.split("```", -1).length should be (5) // 2 code fences -> 4 markers + surrounding splits
  }

  it should "convert = Title = to # Title" in {
    val in = "= My Title ="
    WikidocToMarkdown.migrate(in) should be("# My Title")
  }

  it should "convert == Section == to ## Section" in {
    val in = "== My Section =="
    WikidocToMarkdown.migrate(in) should be("## My Section")
  }

  it should "convert === Subsection === to ### Subsection" in {
    val in = "=== My Subsection ==="
    WikidocToMarkdown.migrate(in) should be("### My Subsection")
  }

  it should "preserve leading whitespace in heading conversions" in {
    val in = "  == Indented Section =="
    WikidocToMarkdown.migrate(in) should be("  ## Indented Section")
  }

  it should "convert multiple heading levels in one document" in {
    val in = """= Title =
              |Some intro text.
              |== Section One ==
              |Section one content.
              |=== Subsection ===
              |More details.
              |== Section Two ==
              |Another section.""".stripMargin
    val out = WikidocToMarkdown.migrate(in)
    out should include("# Title")
    out should include("## Section One")
    out should include("### Subsection")
    out should include("## Section Two")
    out should not include "="
  }

  it should "not convert headings inside code blocks" in {
    val in = """{{{
              |= Not A Title =
              |}}}""".stripMargin
    val out = WikidocToMarkdown.migrate(in)
    out should include("= Not A Title =")
    out should not include "# Not A Title"
  }