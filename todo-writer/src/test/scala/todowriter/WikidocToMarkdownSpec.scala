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

  it should "leave wikilinks with display text unchanged when target is not a URL" in {
    val in = "See [[scala.Double the double type]] for more."
    WikidocToMarkdown.migrate(in) should include("[[scala.Double the double type]]")
  }

  it should "leave code artifact wikilinks unchanged" in {
    // Links to Scala code artifacts should not be converted to markdown links
    // because the target (e.g., scala.collection.immutable.Vector) is not a valid URL
    val in = "Returns an immutable [[scala.collection.immutable.Vector Vector]]."
    WikidocToMarkdown.migrate(in) should include("[[scala.collection.immutable.Vector Vector]]")
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

  it should "be idempotent (running migrate twice yields the same output)" in {
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

  it should "migrateScaladocInner stable for triple-brace example from Exception.scala (idempotent)" in {
    val inner =
      """
       *  {{{
       *    handling(classOf[MalformedURLException], classOf[NullPointerException]) by (_.printStackTrace)
       *  }}}
       *  @group dsl
       """.stripMargin

    val first = WikidocToMarkdown.migrateScaladocInner(inner)
    val second = WikidocToMarkdown.migrateScaladocInner(first)
    first should be (second)
  }

  it should "preserve exact spacing before triple-brace markers in scaladoc migration" in {
    val inner =
      """
       *  Applies fallback function where this partial function is not defined.
       *
       *  Note that expression `pf.applyOrElse(x, default)` is equivalent to
       *   {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
       *
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

    leadingSpacesAfterStar(origMarkerLine) should be (leadingSpacesAfterStar(migMarkerLine))
  }

  it should "not insert additional star-only blank lines when migrating scaladoc" in {
    val tmp = java.nio.file.Files.createTempDirectory("migrate-test")
    try
      val file = tmp.resolve("Boolean.scala")
      val original =
        """package example
          |
          |/**
          | *
          | * Compares two Boolean expressions ''italic'' and returns `true` if they evaluate to a different value.
          | *
          | */
          |object X
          |""".stripMargin
      java.nio.file.Files.writeString(file, original)

      // Simulate performMigration for the single file by applying WikidocToMarkdown.migrateScaladocInner
      val text = java.nio.file.Files.readString(file)
      val pattern = java.util.regex.Pattern.compile("(?s)/\\*\\*(.*?)\\*/")
      val m = pattern.matcher(text)
      val sb = new StringBuffer
      var any = false
      while m.find() do
        val inner = m.group(1)
        val migrated = WikidocToMarkdown.migrateScaladocInner(inner)
        if migrated != inner then
          any = true
          val replacement = "/**" + migrated + "*/"
          m.appendReplacement(sb, java.util.regex.Matcher.quoteReplacement(replacement))
      m.appendTail(sb)
      if any then java.nio.file.Files.writeString(file, sb.toString)

      val content = java.nio.file.Files.readString(file)
      // There must NOT be an extra blank line between two star-only lines introduced by migration.
      val doubleBlank = java.util.regex.Pattern.compile("(?m)^\\s*\\*\\s*$\\r?\\n\\s*\\r?\\n\\s*\\*\\s*")
      doubleBlank.matcher(content).find() should be (false)
    finally
      // cleanup
      def deleteRec(p: java.nio.file.Path): Unit =
        if java.nio.file.Files.isDirectory(p) then
          val it = java.nio.file.Files.list(p).iterator()
          while it.hasNext do deleteRec(it.next())
          java.nio.file.Files.delete(p)
        else java.nio.file.Files.delete(p)
      deleteRec(tmp)
  }

  it should "not add extra space before inline triple-brace markers in scaladoc migration" in {
    val inner =
      """
       *  Applies fallback function where this partial function is not defined.
       *
       *  Note that expression `pf.applyOrElse(x, default)` is equivalent to
       *   {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
       *
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

    leadingSpacesAfterStar(origMarkerLine) should be (leadingSpacesAfterStar(migMarkerLine))
  }
