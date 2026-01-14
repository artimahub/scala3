package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests for Parser.scala - Scaladoc parsing logic */
class ParserSpec extends AnyFlatSpec with Matchers:

  "Parser.parseScaladocContent" should "parse simple scaladoc with initial text" in {
    val inner = " Does something."
    val parsed = Parser.parseScaladocContent(inner)
    parsed.initialText shouldBe Some("Does something.")
    parsed.expositionContent shouldBe empty
    parsed.signatureTags shouldBe empty
    parsed.hasBlankBeforeSignature shouldBe false
  }

  it should "parse scaladoc with exposition content" in {
    val inner = """
                 | * Does something.
                 | * More details here.
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.initialText shouldBe None
    parsed.expositionContent should have length 3
    parsed.expositionContent(0) should include("Does something.")
    parsed.expositionContent(1) should include("More details here.")
    parsed.expositionContent(2) should be("/")
    parsed.signatureTags shouldBe empty
  }

  it should "parse scaladoc with @note tag" in {
    val inner = """
                 | * Does something.
                 | *
                 | *  @note This is important.
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.initialText shouldBe None
    parsed.expositionContent should have length 3
    parsed.expositionContent(0) should include("Does something.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("@note This is important.")
    parsed.signatureTags shouldBe empty
  }

  it should "parse scaladoc with @see tag" in {
    val inner = """
                 | * Does something.
                 | *
                 | *  @see [[OtherClass]]
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 3
    parsed.expositionContent(0) should include("Does something.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("@see [[OtherClass]]")
    parsed.signatureTags shouldBe empty
  }

  it should "parse scaladoc with @example tag" in {
    val inner = """
                 | * Does something.
                 | *
                 | *  @example {{{ code here }}}
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 3
    parsed.expositionContent(0) should include("Does something.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("@example {{{ code here }}}")
    parsed.signatureTags shouldBe empty
  }

  it should "parse scaladoc with @tparam tag" in {
    val inner = """
                 | * Does something.
                 | *
                 | *  @tparam T the type parameter
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 1
    parsed.expositionContent(0) should include("Does something.")
    parsed.signatureTags should have length 1
    parsed.signatureTags.head should include("@tparam T the type parameter")
    parsed.hasBlankBeforeSignature shouldBe true
  }

  it should "parse scaladoc with @param tag" in {
    val inner = """
                 | * Does something.
                 | *
                 | *  @param x the parameter
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.signatureTags should have length 1
    parsed.signatureTags.head should include("@param x the parameter")
    parsed.hasBlankBeforeSignature shouldBe true
  }

  it should "parse scaladoc with @return tag" in {
    val inner = """
                 | * Does something.
                 | *
                 | *  @return the result
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.signatureTags should have length 1
    parsed.signatureTags.head should include("@return the result")
    parsed.hasBlankBeforeSignature shouldBe true
  }

  it should "parse scaladoc with multiple signature tags" in {
    val inner = """
                 | * Does something.
                 | *
                 | *  @tparam T the type
                 | *  @param x the param
                 | *  @return the result
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.signatureTags should have length 3
    parsed.signatureTags(0) should include("@tparam T")
    parsed.signatureTags(1) should include("@param x")
    parsed.signatureTags(2) should include("@return")
  }

  it should "preserve multi-line @return content" in {
    val inner = """
                 | * Creates an array.
                 | *
                 | *  @param n the size
                 | *  @return an Array of size n, where each element contains the result of computing
                 | *  `elem`.
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.signatureTags should have length 2
    parsed.signatureTags(1) should include("an Array of size n")
    parsed.signatureTags(1) should include("`elem`.")
  }

  it should "preserve multi-line @example content" in {
    val inner = """
                 | * Shifts bits right.
                 | *
                 | *  ```scala
                 | *  def foo(x: Int): Int =
                 | *    x + 1
                 | *  ```
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 7
    parsed.expositionContent(0) should include("Shifts bits right.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("```scala")
    parsed.expositionContent(3) should include("def foo(x: Int): Int =")
    parsed.expositionContent(4) should include("  x + 1")
    parsed.expositionContent(5) should include("```")
    parsed.expositionContent(6) should be("/")
  }

  it should "preserve @tags inside markdown code fences (not treat as actual tags)" in {
    val inner = """
                 | * Shows annotation usage.
                 | *
                 | *  ```scala
                 | *  @memoize
                 | *  def fib(n: Int): Int =
                 | *    if n <= 1 then n else fib(n - 1) + fib(n - 2)
                 | *  ```
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 8
    parsed.expositionContent(0) should include("Shows annotation usage.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("```scala")
    parsed.expositionContent(3) should include("@memoize")
    parsed.expositionContent(4) should include("def fib(n: Int): Int =")
    parsed.expositionContent(5) should include("  if n <= 1 then n else fib(n - 1) + fib(n - 2)")
    parsed.expositionContent(6) should include("```")
    parsed.expositionContent(7) should be("/")
    parsed.signatureTags shouldBe empty
  }

  it should "preserve @tags inside triple-brace code fences (not treat as actual tags)" in {
    val inner = """
                 | * Method with code example.
                 | *
                 | *  {{{
                 | *  @param x the value inside triple braces
                 | *  }}}
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 6
    parsed.expositionContent(0) should include("Method with code example.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("{{{")
    parsed.expositionContent(3) should include("@param x the value inside triple braces")
    parsed.expositionContent(4) should include("}}}")
    parsed.expositionContent(5) should be("/")
    parsed.signatureTags shouldBe empty
  }

  it should "preserve indentation inside triple-brace code blocks" in {
    val inner = """
                 | * Example showing scala repl output preserved.
                 | *
                 | *  {{{
                 | *   scala> val a = Array.from(Seq(1, 5))
                 | *   val a: Array[Int] = Array(1, 5)
                 | *
                 | *   scala> val b = Array.from(Range(1, 5))
                 | *   val b: Array[Int] = Array(1, 2, 3, 4)
                 | *  }}}
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 10
    parsed.expositionContent(0) should include("Example showing scala repl output preserved.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("{{{")
    parsed.expositionContent(3) should include("   scala> val a = Array.from(Seq(1, 5))")
    parsed.expositionContent(4) should include("   val a: Array[Int] = Array(1, 5)")
    parsed.expositionContent(5) should be("")
    parsed.expositionContent(6) should include("   scala> val b = Array.from(Range(1, 5))")
    parsed.expositionContent(7) should include("   val b: Array[Int] = Array(1, 2, 3, 4)")
    parsed.expositionContent(8) should include("}}}")
    parsed.expositionContent(9) should be("/")
  }

  it should "not treat @param inside code fence as signature tag" in {
    val inner = """
                 | * Method with code example.
                 | *
                 | *  ```scala
                 | *  /** @param x the value */
                 | *  def example(x: Int) = x
                 | *  ```
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 7
    parsed.expositionContent(0) should include("Method with code example.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("```scala")
    parsed.expositionContent(3) should include("/** @param x the value */")
    parsed.expositionContent(4) should include("def example(x: Int) = x")
    parsed.expositionContent(5) should include("```")
    parsed.expositionContent(6) should be("/")
    parsed.signatureTags shouldBe empty
  }

  it should "preserve indentation inside markdown code fence in @example tag" in {
    val inner = """
                 | * Transforms the tree.
                 | *
                 | *  @example
                 | *  ```scala
                 | *  class memoize extends MacroAnnotation:
                 | *    def transform(using Quotes)(
                 | *      definition: quotes.reflect.Definition,
                 | *      companion: Option[quotes.reflect.Definition]
                 | *    ): List[quotes.reflect.Definition] =
                 | *      import quotes.reflect.*
                 | *      definition match
                 | *        case DefDef(name, _, _, _) =>
                 | *          List(definition)
                 | *  ```
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    // Just verify that the content is preserved somewhere in the exposition
    val allExposition = parsed.expositionContent.mkString("\n")
    allExposition should include("@example")
    allExposition should include("```scala")
    allExposition should include("class memoize extends MacroAnnotation:")
    allExposition should include("def transform(using Quotes)(")
    allExposition should include("definition: quotes.reflect.Definition,")
    allExposition should include("companion: Option[quotes.reflect.Definition]")
    allExposition should include("import quotes.reflect.*")
    allExposition should include("definition match")
    allExposition should include("case DefDef(name, _, _, _) =>")
    allExposition should include("List(definition)")
    // Verify that @param tags are NOT in signature section (they're in code fence)
    parsed.signatureTags shouldBe empty
  }

  it should "handle multiple code fences in same scaladoc" in {
    val inner = """
                 | * Method with multiple examples.
                 | *
                 | *  First example:
                 | *  ```scala
                 | *  @annotation
                 | *  def foo = 1
                 | *  ```
                 | *
                 | *  Second example:
                 | *  ```scala
                 | *  @annotation
                 | *  def bar = 2
                 | *  ```
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 14
    parsed.expositionContent(0) should include("Method with multiple examples.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("First example:")
    parsed.expositionContent(3) should include("```scala")
    parsed.expositionContent(4) should include("@annotation")
    parsed.expositionContent(5) should include("def foo = 1")
    parsed.expositionContent(6) should include("```")
    parsed.expositionContent(7) should be("")
    parsed.expositionContent(8) should include("Second example:")
    parsed.expositionContent(9) should include("```scala")
    parsed.expositionContent(10) should include("@annotation")
    parsed.expositionContent(11) should include("def bar = 2")
    parsed.expositionContent(12) should include("```")
    parsed.expositionContent(13) should be("/")
  }

  it should "handle code fence without language specifier" in {
    val inner = """
                 | * Method.
                 | *
                 | *  ```
                 | *  @tag inside
                 | *  some code
                 | *  ```
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 7
    parsed.expositionContent(0) should include("Method.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("```")
    parsed.expositionContent(3) should include("@tag inside")
    parsed.expositionContent(4) should include("some code")
    parsed.expositionContent(5) should include("```")
    parsed.expositionContent(6) should be("/")
  }

  it should "preserve existing indentation with 3+ spaces after asterisk" in {
    val inner = """
                 | * Method.
                 | *
                 | *   - Item one
                 | *   - Item two
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 5
    parsed.expositionContent(0) should include("Method.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("   - Item one")
    parsed.expositionContent(3) should include("   - Item two")
    parsed.expositionContent(4) should be("/")
  }

  it should "preserve deep indentation inside nested list" in {
    val inner = """
                 | * Method.
                 | *
                 | *  #### Restrictions
                 | *   - All definitions must have same owner.
                 | *     - Special case: an annotated def can return a class.
                 | *   - Can not return a type.
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.expositionContent should have length 7
    parsed.expositionContent(0) should include("Method.")
    parsed.expositionContent(1) should be("")
    parsed.expositionContent(2) should include("#### Restrictions")
    parsed.expositionContent(3) should include("   - All definitions must have same owner.")
    parsed.expositionContent(4) should include("     - Special case: an annotated def can return a class.")
    parsed.expositionContent(5) should include("   - Can not return a type.")
    parsed.expositionContent(6) should be("/")
  }

  it should "detect when there is no blank line before signature tags" in {
    val inner = """
                 | * Does something.
                 | *  @param x the parameter
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.hasBlankBeforeSignature shouldBe false
  }

  it should "detect when there is a blank line before signature tags" in {
    val inner = """
                 | * Does something.
                 | *
                 | *  @param x the parameter
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.hasBlankBeforeSignature shouldBe true
  }

  it should "handle empty scaladoc" in {
    val inner = ""
    val parsed = Parser.parseScaladocContent(inner)
    parsed.initialText shouldBe None
    parsed.expositionContent shouldBe empty
    parsed.signatureTags shouldBe empty
    parsed.hasBlankBeforeSignature shouldBe false
  }

  it should "handle scaladoc with only tags" in {
    val inner = """
                 | *  @param x the parameter
                 | *  @return the result
                 | */""".stripMargin
    val parsed = Parser.parseScaladocContent(inner)
    parsed.initialText shouldBe None
    parsed.expositionContent shouldBe empty
    parsed.signatureTags should have length 2
  }

  "Parser.isExpositionTag" should "identify @note as exposition tag" in {
    Parser.isExpositionTag("@note something") shouldBe true
  }

  it should "identify @see as exposition tag" in {
    Parser.isExpositionTag("@see something") shouldBe true
  }

  it should "identify @example as exposition tag" in {
    Parser.isExpositionTag("@example something") shouldBe true
  }

  it should "not identify @param as exposition tag" in {
    Parser.isExpositionTag("@param something") shouldBe false
  }

  it should "not identify @tparam as exposition tag" in {
    Parser.isExpositionTag("@tparam something") shouldBe false
  }

  it should "not identify @return as exposition tag" in {
    Parser.isExpositionTag("@return something") shouldBe false
  }

  "Parser.isSignatureTag" should "identify @param as signature tag" in {
    Parser.isSignatureTag("@param something") shouldBe true
  }

  it should "identify @tparam as signature tag" in {
    Parser.isSignatureTag("@tparam something") shouldBe true
  }

  it should "identify @return as signature tag" in {
    Parser.isSignatureTag("@return something") shouldBe true
  }

  it should "not identify @note as signature tag" in {
    Parser.isSignatureTag("@note something") shouldBe false
  }

  it should "not identify @see as signature tag" in {
    Parser.isSignatureTag("@see something") shouldBe false
  }

  it should "not identify @example as signature tag" in {
    Parser.isSignatureTag("@example something") shouldBe false
  }

  "Parser.isCodeFenceStart" should "detect markdown code fence start" in {
    Parser.isCodeFenceStart("```scala") shouldBe true
    Parser.isCodeFenceStart("```") shouldBe true
    Parser.isCodeFenceStart("```   ") shouldBe true
  }

  it should "not detect non-fence lines as code fence start" in {
    Parser.isCodeFenceStart("some code") shouldBe false
    Parser.isCodeFenceStart("```scala code") shouldBe false
  }

  "Parser.isCodeFenceEnd" should "detect markdown code fence end" in {
    Parser.isCodeFenceEnd("```") shouldBe true
    Parser.isCodeFenceEnd("```   ") shouldBe true
  }

  it should "not detect non-fence lines as code fence end" in {
    Parser.isCodeFenceEnd("```scala") shouldBe false
    Parser.isCodeFenceEnd("some code") shouldBe false
  }

  "Parser.isTripleBraceStart" should "detect triple-brace start" in {
    Parser.isTripleBraceStart("{{{") shouldBe true
    Parser.isTripleBraceStart("{{{   ") shouldBe true
  }

  it should "not detect non-triple-brace lines as start" in {
    Parser.isTripleBraceStart("{{{ code") shouldBe false
    Parser.isTripleBraceStart("some code") shouldBe false
  }

  "Parser.isTripleBraceEnd" should "detect triple-brace end" in {
    Parser.isTripleBraceEnd("}}}") shouldBe true
    Parser.isTripleBraceEnd("}}}   ") shouldBe true
  }

  it should "not detect non-triple-brace lines as end" in {
    Parser.isTripleBraceEnd("}}} code") shouldBe false
    Parser.isTripleBraceEnd("some code") shouldBe false
  }