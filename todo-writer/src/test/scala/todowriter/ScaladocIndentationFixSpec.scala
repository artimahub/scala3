package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScaladocIndentationFixSpec extends AnyFlatSpec with Matchers:

  "Fixer.applyFixes in skip-todo mode" should "align all '*' columns in scaladoc after fix" in {
    val text = """/**
                 |   * A method that returns its input value.
                 |   * @tparam A type of the input value x.
                 |   * @param x the value of type `A` to be returned.
                 |   * @return the value `x`.
                 | * @group utilities 
                 |   */
                 |  @inline def identity[A](x: A): A = x""".stripMargin

    val block = ScaladocBlock.findAll(text).head
    val declChunk = Declaration.getDeclarationAfter(text, block.endIndex)
    val decl = Declaration.parse(declChunk)
    val issues = ScaladocChecker.validate(block, decl)
    // In skip-todo mode we want to exercise alignment-only changes even if no missing tags.
    // Do not require issues to be non-empty.

    val (newText, count) = Fixer.applyFixes(text, List(CheckResult(block, decl, issues)), insertTodo = false)
    count should be > 0

    val newBlock = ScaladocBlock.findAll(newText).head
    val formatted = Fixer.buildFixedBlock(newText, newBlock, Nil, Nil, false)
    val lines = formatted.split("\n")

    // All continuation lines (between opening and closing) that contain '*' should have the '*' in the same column.
    val starCols = lines.drop(1).dropRight(1).filter(_.contains("*")).map(_.indexOf('*')).distinct
    starCols.size should be (1)
  }