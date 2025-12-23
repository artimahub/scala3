package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AsteriskAlignmentSpec extends AnyFlatSpec with Matchers:

  "Fixer.buildFixedBlock" should "convert two-space asterisk alignment to one-space" in {
    // Include a following method definition to ensure leading indentation context exists.
    val text = """/**
                 |  * Testing
                 |  */
                 |def foo(): Unit = ()""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, Nil, false)
    val lines = result.split("\n")

    // Expect multi-line output:
    val expected = """/**
                     | *  Testing
                     | */""".stripMargin
    result should be(expected)
  }

  it should "convert indented one-space asterisk alignment to two-space after leading indentation" in {
    // Indented scaladoc followed by an indented method definition.
    val text =   """    /**
                   |     * Testing
                   |     */
                   |    def bar(x: Int): Int = ???""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, Nil, false)
    val lines = result.split("\n")

    // The asterisk line should be aligned relative to the method indentation.
    val expected = """   /**
                     |    *  Testing
                     |    */
                     |   def bar(x: Int): Int = ???""".stripMargin
    result should be(expected)
  }