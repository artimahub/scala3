package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DuplicationAndAsteriskSpec extends AnyFlatSpec with Matchers:

  "Fixer.applyFixes" should "not duplicate or remove the declaration when fixing a single-line scaladoc with typeparams" in {
    val text =
      """/** Provides an implicit conversion from the Array object to a collection Factory. */
        |implicit def toFactory[A : ClassTag](dummy: Array.type): Factory[A, Array[A]] = new ArrayFactory(dummy)""".stripMargin

    val block = ScaladocBlock.findAll(text).head
    val chunk = Declaration.getDeclarationAfter(text, block.endIndex)
    val decl = Declaration.parse(chunk)
    val result = CheckResult(block, decl, List(Issue.MissingTparam(List("A"))))

    val (newText, count) = Fixer.applyFixes(text, List(result))

    // The declaration should appear exactly once in the resulting text
    val declOccurrences = newText.split("\n").count(_.contains("implicit def toFactory"))
    declOccurrences should be(1)

    // Asterisks should align under the first '*' of the opening line in the fixed block
    val fixedBlock = ScaladocBlock.findAll(newText).head
    val fixed = Fixer.buildFixedBlock(newText, fixedBlock, Nil, Nil, false)
    val lines = fixed.split("\n")
    lines.drop(1).dropRight(1).foreach { line =>
      line should startWith(" *")
    }
  }