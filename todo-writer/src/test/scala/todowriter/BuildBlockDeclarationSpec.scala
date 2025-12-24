package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BuildBlockDeclarationSpec extends AnyFlatSpec with Matchers:

  "Fixer.buildFixedBlock" should "not include the following declaration and should align asterisks under the first '*' of the opening line" in {
    val text =
      """/** Provides an implicit conversion from the Array object to a collection Factory. */
        |implicit def toFactory[A : ClassTag](dummy: Array.type): Factory[A, Array[A]] = new ArrayFactory(dummy)""".stripMargin

    val block = ScaladocBlock.findAll(text).head

    // Call buildFixedBlock directly to observe its output
    val result = Fixer.buildFixedBlock(text, block, List("A"), List("dummy"), false)

    // The result should NOT contain the declaration (buildFixedBlock should only return the comment block)
    result.contains("implicit def toFactory") should be(false)

    // Asterisk lines should align under the first '*' of the opening "/**" line
    val lines = result.split("\n")
    lines.head.trim.startsWith("/**") should be(true)
    // check that subsequent non-empty star lines start with a single space followed by '*'
    lines.drop(1).dropRight(1).foreach { line =>
      line should startWith(" *")
    }
  }