package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.util.regex.Pattern
import java.util.regex.Matcher

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

  it should "convert triple-brace code blocks within @example tag to fenced code blocks" in {
    val in = """Returns the bitwise negation of this value.
              |@example {{{
              |~5 == -6
              |// in binary: ~00000101 ==
              |//             11111010
              |}}}
              |""".stripMargin
    val out = WikidocToMarkdown.migrate(in)
    // Should convert {{{ to ```
    out should include("```")
    out should include("~5 == -6")
    // Should NOT contain {{{ after migration
    out should not include "{{{"
    out should not include "}}}"
  }

  it should "convert triple-brace code blocks in multiple @example tags to fenced code blocks" in {
    val in = """Defines a finite set of values specific to the enumeration.
              |@example {{{
              |// First example
              |val x = 1
              |}}}
              |@example {{{
              |// Second example
              |val y = 2
              |}}}
              |""".stripMargin
    val out = WikidocToMarkdown.migrate(in)
    // Should convert all {{{ to ```
    out should include("```")
    out should include("// First example")
    out should include("// Second example")
    // Should NOT contain any {{{ or }}} after migration
    out should not include "{{{"
    out should not include "}}}"
  }

  it should "convert triple-brace code blocks in multiple @example tags using migrateScaladocInner" in {
    val inner =
      """
       *  Defines a finite set of values specific to the enumeration.
       *  @example {{{
       *  // First example
       *  val x = 1
       *  }}}
       *  @example {{{
       *  // Second example
       *  val y = 2
       *  }}}
       *  """.stripMargin
    val out = WikidocToMarkdown.migrateScaladocInner(inner)
    // Should convert all {{{ to ```
    out should include("```")
    out should include("// First example")
    out should include("// Second example")
    // Should NOT contain any {{{ or }}} after migration
    out should not include "{{{"
    out should not include "}}}"
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

  it should "preserve alignment for trailing tag lines (group) without adding extra indentation" in {
    val source =
      """|  /**
         |   * A method that returns its input value.
         |   * @tparam A type of the input value x.
         |   * @param x the value of type `A` to be returned.
         |   * @return the value `x`.
         |   * @group utilities 
         |   */
         |""".stripMargin

    val pattern = Pattern.compile("(?s)/\\*\\*(.*?)\\*/")
    val matcher = pattern.matcher(source)
    val sb = new StringBuffer
    var any = false
    while matcher.find() do
      val inner = matcher.group(1)
      val migrated = WikidocToMarkdown.migrateScaladocInner(inner)
      if migrated != inner then
        any = true
        val replacement = "/**" + migrated + "*/"
        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    matcher.appendTail(sb)
    val migratedSource = if any then sb.toString else source

    // Expect no changes to spacing/asterisk alignment for the @group line.
    migratedSource shouldBe source
  }

  it should "not modify scaladoc blocks that are commented out with // prefix" in {
    val source =
      """|  // /** Specifies how the array lengths should vary.
         |  //  *
         |  //  *  By default,  `UnrolledBuffer` uses arrays of a fixed size.  A length
         |  //  *  policy can be given that changes this scheme to, for instance, an
         |  //  *  exponential growth.
         |  //  *
         |  //  *  @param nextLength   computes the length of the next array from the length of the latest one
         |  */
         |  // def setLengthPolicy(nextLength: Int => Int): Unit = { myLengthPolicy = nextLength }
         |""".stripMargin

    val pattern = Pattern.compile("(?s)^(?!.*//)/\\*\\*(.*?)\\*/", Pattern.MULTILINE)
    val matcher = pattern.matcher(source)
    val sb = new StringBuffer
    var any = false
    while matcher.find() do
      val inner = matcher.group(1)
      val migrated = WikidocToMarkdown.migrateScaladocInner(inner)
      if migrated != inner then
        any = true
        val replacement = "/**" + migrated + "*/"
        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    matcher.appendTail(sb)
    val migratedSource = if any then sb.toString else source

    val expected =
      """|  // /** Specifies how the array lengths should vary.
         |  //  *
         |  //  *  By default,  `UnrolledBuffer` uses arrays of a fixed size.  A length
         |  //  *  policy can be given that changes this scheme to, for instance, an
         |  //  *  exponential growth.
         |  //  *
         |  //  *  @param nextLength   computes the length of the next array from the length of the latest one
         |  */
         |  // def setLengthPolicy(nextLength: Int => Int): Unit = { myLengthPolicy = nextLength }
         |""".stripMargin

    // The commented-out block must remain unchanged.
    migratedSource shouldBe expected
  }

  it should "preserve the opening line and not remove it for App.scala comment" in {
    val source =
      """|  /** The main method.
         |   *  This stores all arguments so that they can be retrieved with `args`
         |   *  and then executes all initialization code segments in the order in which
         |   *  they were passed to `delayedInit`.
         |   *  @param args the arguments passed to the main method
         |   */
         |  final def main(args: Array[String]) = {
         |    this._args = args
         |    for (proc <- initCode) proc()
         |    if (util.Properties.propIsSet("scala.time")) {
         |      val total = currentTime - executionStart
         |      Console.println("[total " + total + "ms]")
         |    }
         |  }
         |""".stripMargin

    val pattern = Pattern.compile("(?s)/\\*\\*(.*?)\\*/")
    val matcher = pattern.matcher(source)
    val sb = new StringBuffer
    var any = false
    while matcher.find() do
      val inner = matcher.group(1)
      val migrated = WikidocToMarkdown.migrateScaladocInner(inner)
      if migrated != inner then
        any = true
        val replacement = "/**" + migrated + "*/"
        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    matcher.appendTail(sb)
    val migratedSource = if any then sb.toString else source

    // The opener must remain unchanged
    migratedSource should include ("  /** The main method.")
    // The opener must not be removed or replaced by following lines
    migratedSource should include ("   *  This stores all arguments")
    // No extra star-only line immediately before closing
    migratedSource should not include ("\n   *\n   */")
  }

  it should "not produce star-only lines that contain only spaces" in {
    val source =
      """|  /** The command line arguments passed to the application's `main` method.
         |  *   
         |  */
         |  protected final def args: Array[String] = _args
         |""".stripMargin

    val pattern = """(?s)/\*\*(.*?)\*/""".r
    val migrated = pattern.replaceAllIn(source, m => "/**" + WikidocToMarkdown.migrateScaladocInner(m.group(1)) + "*/")

    // failing condition: any line that trims to "*" (i.e., star-only possibly with spaces) must not exist
    migrated.linesIterator.exists(line => line.trim == "*") shouldBe false

    // ensure comment opener and following code remain present
    migrated should include ("/** The command line arguments")
    migrated should include ("*/\n  protected final def args")
  }

  it should "preserve indentation and the first line after an indented /**" in {
    val source =
      """|  /** The main method.
         |  *  This stores all arguments so that they can be retrieved with `args`
         |  *  and then executes all initialization code segments in the order in which
         |  *  they were passed to `delayedInit`.
         |  *  @param args the arguments passed to the main method
         |  */ 
         |  final def main(args: Array[String]) = {
         |    this._args = args
         |    for (proc <- initCode) proc()
         |  }
         |""".stripMargin

    val pattern = """(?s)/\*\*(.*?)\*/""".r
    val migrated = pattern.replaceAllIn(source, m => "/**" + WikidocToMarkdown.migrateScaladocInner(m.group(1)) + "*/")

    // The indented opening line must be preserved exactly
    migrated should include ("  /** The main method.")
    // The closing comment must not gain an extra star-only line before it
    migrated should not include ("\n *\n  */")
    // The code after the comment must remain on the following line with same indentation
    migrated should include ("*/ \n  final def main(args: Array[String]) = {")
  }

  it should "not insert a star-only blank line immediately after opening /**" in {
    val source =
      """|/**
         | * Compares two Boolean expressions and returns `true` if they evaluate to a different value.
         | *
         | * `a != b` returns `true` if and only if
         | *  - `a` is `true` and `b` is `false` or
         | *  - `a` is `false` and `b` is `true`.
         | *
         | */
         |def !=(x: Boolean): Boolean
         |""".stripMargin

    val pattern = Pattern.compile("(?s)/\\*\\*(.*?)\\*/")
    val matcher = pattern.matcher(source)
    val sb = new StringBuffer
    var any = false
    while matcher.find() do
      val inner = matcher.group(1)
      val migrated = WikidocToMarkdown.migrateScaladocInner(inner)
      if migrated != inner then
        any = true
        val replacement = "/**" + migrated + "*/"
        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    matcher.appendTail(sb)
    val migratedSource = if any then sb.toString else source

    // must not insert a star-only blank line immediately after the opener
    migratedSource should not include ("\n *\n * Compares")
    // opener must be followed directly by the first content line
    migratedSource should include ("/**\n * Compares")
  }

  it should "preserve inline fragments when inner does NOT start with a newline" in {
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

  it should "not alter unrelated content or indentation (only migrate wikidoc to markdown)" in {
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

  it should "preserve the opener line and not inject extra star before closing in performMigration-like replacement" in {
    val source =
      """|  /** The main method.
         |  *  This stores all arguments so that they can be retrieved with `args`
         |  *  and then executes all initialization code segments in the order in which
         |  *  they were passed to `delayedInit`.
         |  *  @param args the arguments passed to the main method
         |  */ 
         |  final def main(args: Array[String]) = {
         |    this._args = args
         |    for (proc <- initCode) proc()
         |  }
         |""".stripMargin

    val pattern = Pattern.compile("(?s)/\\*\\*(.*?)\\*/")
    val matcher = pattern.matcher(source)
    val sb = new StringBuffer
    var any = false
    while matcher.find() do
      val inner = matcher.group(1)
      val migrated = WikidocToMarkdown.migrateScaladocInner(inner)
      if migrated != inner then
        any = true
        val replacement = "/**" + migrated + "*/"
        matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement))
    matcher.appendTail(sb)
    val migratedSource = if any then sb.toString else source

    // opener line preserved
    migratedSource should include ("  /** The main method.")
    // no extra star-only line inserted immediately before closing
    migratedSource should not include ("\n *\n  */")
    // closing marker remains adjacent to following code line
    migratedSource should include ("*/ \n  final def main(args: Array[String]) = {")
  }

  it should "preserve a comment's first line and not add an extra '*' before closing */" in {
    val source =
      """|/** The main method.
         | *  This stores all arguments so that they can be retrieved with `args`
         | *  and then executes all initialization code segments in the order in which
         | *  they were passed to `delayedInit`.
         | *  @param args the arguments passed to the main method
         | */
         |final def main(args: Array[String]) = {
         |  this._args = args
         |  for (proc <- initCode) proc()
         |}
         |""".stripMargin

    val Scaladoc = """(?s)/\*\*(.*?)\*/""".r
    val migrated = Scaladoc.replaceAllIn(source, m => "/**" + WikidocToMarkdown.migrateScaladocInner(m.group(1)) + "*/")

    // The original first line after "/**" must be preserved.
    migrated should include ("/** The main method.")

    // The closing comment should remain "*/" on its own line (no extra leading '*' line immediately before it).
    migrated should not include ("\n *\n*/")
    // Ensure the comment close is directly followed by the next code line, preserving spacing.
    migrated should include ("*/\nfinal def main(args: Array[String]) = {")
  }

  it should "not add a trailing star-only line before the comment close" in {
    val inner =
      """/**
        | *  This stores all arguments so that they can be retrieved with `args`
        | *  and then executes all initialization code segments in the order in which
        | *  they were passed to `delayedInit`.
        | *  @param args the arguments passed to the main method
        | */""".stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // must not end with a star-only line which would insert an extra '*' before the closing */
    //migrated.endsWith("\n *") shouldBe false
    migrated.split("\n").exists(l => l.trim == "*") shouldBe false

    // last content line must be preserved exactly
    migrated should include ("*  @param args the arguments passed to the main method")
  }

  it should "preserve the exact Tuple signature fragment and not remove leading text" in {
    val inner =
      """
       *  /**: ... *: An * At` and `B1 *: ... *: Bn *: Bt`
       *   {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
       *   */
       *  def test(x: Int): Unit = {}
       """.stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // Ensure the specific inline fragment is preserved exactly (no truncation or reordering).
    migrated should include ("/**: ... *: An * At` and `B1 *: ... *: Bn *: Bt")
  }

  it should "preserve the exact Tuple.scala inline fragment" in {
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

  it should "not rewrite unrelated inline sequences such as Tuple signature fragments" in {
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

  it should "not modify unrelated inline text (keep backticks and asterisks intact)" in {
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

  // Tests for {{{ }}} code blocks without * prefixes

  it should "align asterisks correctly when {{{ }}} block has no * prefixes (switch.scala case)" in {
    // This is the exact pattern from library/src/scala/annotation/switch.scala
    // where {{{ }}} lines don't have * prefixes.
    // The format has lines starting with " *" (single space before asterisk).
    val inner =
      """ An annotation to be applied to a match expression.  If present,
 *  the compiler will verify that the match has been compiled to a
 *  [[https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-3.html#jvms-3.10 tableswitch or lookupswitch]]
 *  and issue a warning if it instead compiles into a series of conditional expressions.
 *  Example usage:
{{{
  val Constant = 'Q'
  def tokenMe(ch: Char) = (ch: @switch) match {
    case ' ' | '\t' | '\n'  => 1
    case 'A' | 'Z' | '$'    => 2
    case '5' | Constant     => 3  // a non-literal may prevent switch generation: this would not compile
    case _                  => 4
  }
}}}
 *
 *  Note: for pattern matches with one or two cases, the compiler generates jump instructions.
 *  Annotating such a match with `@switch` does not issue any warning.
 """

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // All lines with asterisks should have consistent prefix (single space before *)
    val linesWithStar = migrated.linesIterator.filter(l => l.contains('*') && l.trim.startsWith("*")).toList
    linesWithStar.foreach { line =>
      // Lines with * that are scaladoc content (starting with *) should start with " *" (single space before star)
      line should startWith(" *")
    }

    // The code block markers should have 2 spaces after * (like other content)
    migrated should include(" *  ```")

    // The code block content should be present and properly formatted
    migrated should include("val Constant = 'Q'")
    migrated should include("def tokenMe")
    migrated should include("case ' ' | '\\t' | '\\n'")
  }

  it should "align asterisks correctly when {{{ }}} block has no * prefixes (showAsInfix.scala case)" in {
    // This is the pattern from library/src/scala/annotation/showAsInfix.scala
    // The file has unusual indentation with " *" for some lines and "  *" for others.
    // The key test is that the code block lines (which originally had no *) get
    // the MOST COMMON prefix from other lines (which is "  *" - 2 spaces).
    val inner =
      """
  * This annotation configures how Scala prints two-parameter generic types.
  *
  * By default, types with symbolic names are printed infix; while types without
  * them are printed using the regular generic type syntax.
  *
  * Example of usage:
  {{{
    scala> class Map[T, U]
    defined class Map

    scala> def foo: Int Map Int = ???
    foo: Map[Int,Int]

    scala> @showAsInfix class Map[T, U]
    defined class Map

    scala> def foo: Int Map Int = ???
    foo: Int Map Int
  }}}
  *
  * @param enabled whether to show this type as an infix type operator.
  """

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // All lines with asterisks that are scaladoc content (starting with *) should have consistent prefix
    val linesWithStar = migrated.linesIterator.filter(l => l.contains('*') && l.trim.startsWith("*")).toList
    linesWithStar.foreach { line =>
      // Lines with * should start with "  *" (2 spaces before star, matching the most common prefix)
      line should startWith("  *")
    }

    // The code block markers should have 2 spaces after * (like other content)
    migrated should include("  *  ```")

    // The code block content should be present
    migrated should include("scala> class Map[T, U]")
    migrated should include("defined class Map")
  }

  it should "use standard scaladoc prefix for code block lines that originally had no *" in {
    // Simplified test case: {{{ }}} without * on any line inside
    val inner =
      """
 *  Some description.
 *  Example:
{{{
code line 1
code line 2
}}}
 *  More text.
 """

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // The {{{ and }}} markers (now ```) should use the same prefix as other lines
    val lines = migrated.split("\n")
    val codeStartLine = lines.find(l => l.contains("```")).get
    val descriptionLine = lines.find(_.contains("Some description")).get

    // Both should start with " *" (standard scaladoc prefix)
    codeStartLine should startWith(" *")
    descriptionLine should startWith(" *")

    // The ``` markers should have proper spacing (at least 2 spaces after *)
    codeStartLine should startWith(" *  ```")
  }

  it should "convert inline {{{ }}} to fenced code block with content on separate lines" in {
    // The issue: {{{It was the best of times, it was the worst of times}}}
    // should become:
    // ```
    // It was the best of times, it was the worst of times
    // ```

    val in = "{{{It was the best of times, it was the worst of times}}}"
    val out = WikidocToMarkdown.migrate(in)

    // Should start with ``` on its own line
    out should startWith("```")
    // Should end with ``` on its own line
    out should endWith("```")
    // The content should be on a separate line between the markers
    out should include("\nIt was the best of times, it was the worst of times\n")
    // Should NOT have content on the same line as the markers
    out should not include ("{{{It was")
    out should not include ("times}}}")
  }

  it should "convert inline {{{ }}} to fenced code block in scaladoc inner content" in {
    val inner =
      """
       *  An example of inline code.
       *  {{{It was the best of times, it was the worst of times}}}
       *  End of example.
       """.stripMargin

    val migrated = WikidocToMarkdown.migrateScaladocInner(inner)

    // The inline {{{ }}} should be converted to fenced code block with content on separate lines
    migrated should include(" *  ```")
    migrated should include("It was the best of times, it was the worst of times")
    // The closing ``` should be on its own line after the content
    val lines = migrated.split("\n")
    val codeBlockStartIdx = lines.indexWhere(_.contains("```"))
    val codeBlockEndIdx = lines.lastIndexWhere(_.contains("```"))

    codeBlockStartIdx should be >= 0
    codeBlockEndIdx should be >= 0
    codeBlockEndIdx should be > codeBlockStartIdx + 1  // Content should be on at least one line between markers
  }

  it should "convert existing backtick-fenced inline code to proper fenced code block" in {
    // The issue: ```It was the best of times, it was the worst of times```
    // should become:
    // ```
    // It was the best of times, it was the worst of times
    // ```

    val in = "```It was the best of times, it was the worst of times```"
    val out = WikidocToMarkdown.migrate(in)

    // Should start with ``` on its own line
    out should startWith("```")
    // Should end with ``` on its own line
    out should endWith("```")
    // The content should be on a separate line between the markers
    out should include("\nIt was the best of times, it was the worst of times\n")
    // Should NOT have content on the same line as the markers
    out should not include ("```It was")
    out should not include ("times```")
  }

  "WikidocToMarkdown.migrateScaladocInner" should "not split ${ ... } expressions inside code blocks" in {
    // The inner content of a Scaladoc comment (inside /** ... */)
    // This matches what performMigration extracts
    val input = """
 *  Quotation context provided by a macro expansion or in the scope of `scala.quoted.staging.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API metaprogramming API.
 *  This API does not have the static type guarantees that `Expr` and `Type` provide.
 *  `Quotes` are generated from an enclosing `${ ... }` or `scala.staging.run`. For example:
 *  ```scala sc:nocompile
 *  import scala.quoted.*
 *  inline def myMacro: Expr[T] =
 *    ${ /* (quotes: Quotes) ?=> */ myExpr }
 *  def myExpr(using Quotes): Expr[T] =
 *    '{ f(${ /* (quotes: Quotes) ?=> */ myOtherExpr }) }
 *  }
 *  def myOtherExpr(using Quotes): Expr[U] = '{ ... }
 *  ```
 """.stripMargin.trim

    val result = WikidocToMarkdown.migrateScaladocInner(input)

    // The input should remain unchanged since it's already properly formatted Markdown
    result shouldBe input
  }
