package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScaladocDeprecatedAlignSpec extends AnyFlatSpec with Matchers:

  "Fixer.applyFixes in skip-todo mode" should "align '*' under the first '*' of /** for the deprecated example" in {
    val text = """  /** @deprecated Iterables are not guaranteed to have a consistent order, so the `Ordering`
                 |    *             returned by this method may not be stable or meaningful. If you are using a type
                 |    *             with a consistent order (such as `Seq`), use its `Ordering` (found in the
                 |    *             [[Implicits]] object) instead.
                 |    */
                 |  @deprecated("Iterables are not guaranteed to have a consistent order; if using a type with a " +
                 |    "consistent order (e.g. Seq), use its Ordering (found in the Ordering.Implicits object)", since = "2.13.0")
                 |  implicit def Iterable[T](implicit ord: Ordering[T]): Ordering[Iterable[T]] =
                 |    new IterableOrdering[Iterable, T](ord)""".stripMargin

    val expected = """  /** @deprecated Iterables are not guaranteed to have a consistent order, so the `Ordering`
                     |   *             returned by this method may not be stable or meaningful. If you are using a type
                     |   *             with a consistent order (such as `Seq`), use its `Ordering` (found in the
                     |   *             [[Implicits]] object) instead.
                     |   */
                     |  @deprecated("Iterables are not guaranteed to have a consistent order; if using a type with a " +
                     |    "consistent order (e.g. Seq), use its Ordering (found in the Ordering.Implicits object)", since = "2.13.0")
                     |  implicit def Iterable[T](implicit ord: Ordering[T]): Ordering[Iterable[T]] =
                     |    new IterableOrdering[Iterable, T](ord)""".stripMargin

    val block = ScaladocBlock.findAll(text).head
    val declChunk = Declaration.getDeclarationAfter(text, block.endIndex)
    val decl = Declaration.parse(declChunk)
    val issues = ScaladocChecker.validate(block, decl)

    val (newText, count) = Fixer.applyFixes(text, List(CheckResult(block, decl, issues)), insertTodo = false)
    count should be > 0

    val newBlock = ScaladocBlock.findAll(newText).head
    val formatted = Fixer.buildFixedBlock(newText, newBlock, Nil, Nil, false)

    // buildFixedBlock should only return the scaladoc block, not the declaration
    formatted.contains("@deprecated(\"Iterables") should be(false)
    formatted.contains("implicit def Iterable") should be(false)

    // The formatted block should have '*' aligned under the first '*' of the opening "/**" line
    val lines = formatted.split("\n")
    lines.head.trim.startsWith("/**") should be(true)
    // check that all star lines have the '*' at the same column
    val starCols = lines.drop(1).dropRight(1).filter(_.contains("*")).map(_.indexOf('*')).distinct
    starCols.size should be (1)
    // the '*' should be at column 3 (0-indexed) to align under the first '*' of "/**"
    starCols.head should be(3)
  }