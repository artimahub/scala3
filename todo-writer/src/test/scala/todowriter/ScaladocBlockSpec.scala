package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScaladocBlockSpec extends AnyFlatSpec with Matchers:

  "ScaladocBlock" should "extract @param tags" in {
    val text = """/** Does something.
                 | *  @param x the x value
                 | *  @param y the y value
                 | */""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.params should be(List("x", "y"))
  }

  it should "extract @tparam tags" in {
    val text = """/** A generic method.
                 | *  @tparam A the first type
                 | *  @tparam B the second type
                 | */""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.tparams should be(List("A", "B"))
  }

  it should "extract @return tag" in {
    val text = """/** Returns something.
                 | *  @return the result
                 | */""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.hasReturn should be(true)
  }

  it should "handle empty scaladoc" in {
    val text = """/** */"""
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.params should be(empty)
    blocks.head.tparams should be(empty)
    blocks.head.hasReturn should be(false)
  }

  it should "handle scaladoc with no tags" in {
    val text = """/** This is just a description. */"""
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.params should be(empty)
    blocks.head.tparams should be(empty)
    blocks.head.hasReturn should be(false)
  }

  it should "handle single-line scaladoc" in {
    val text = """/** Returns the count. */"""
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.isOneLiner should be(true)
  }

  it should "detect one-liner (single line of descriptive content)" in {
    val text = """/** Returns the current count. */"""
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.isOneLiner should be(true)
  }

  it should "detect one-liner even with @param tags present" in {
    val text = """/** Gets the value for the given key.
                 | *
                 | *  @param key the lookup key
                 | */""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.isOneLiner should be(true)
  }

  it should "detect one-liner when sentence spans multiple physical lines" in {
    val text = """/** Returns a two-dimensional array that contains the results of some element
                 | *  computation a number of times.
                 | *
                 | *  @param n1 the number of elements
                 | */""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.isOneLiner should be(true)
  }

  it should "not consider multiple paragraphs as one-liner" in {
    val text = """/** Computes the result.
                 | *
                 | *  This method performs complex calculation.
                 | */""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.isOneLiner should be(false)
  }

  it should "find multiple scaladoc blocks" in {
    val text = """/** First doc. */
                 |def foo(): Unit = ()
                 |
                 |/** Second doc. */
                 |def bar(): Unit = ()""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 2
  }

  it should "track correct line numbers" in {
    val text = """package test
                 |
                 |/** First doc. */
                 |def foo(): Unit = ()
                 |
                 |/** Second doc. */
                 |def bar(): Unit = ()""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 2
    blocks(0).lineNumber should be(3)
    blocks(1).lineNumber should be(6)
  }

  it should "handle mixed tags" in {
    val text = """/** A method.
                 | *  @tparam T the type
                 | *  @param x the value
                 | *  @return the result
                 | */""".stripMargin
    val blocks = ScaladocBlock.findAll(text)
    blocks should have size 1
    blocks.head.tparams should be(List("T"))
    blocks.head.params should be(List("x"))
    blocks.head.hasReturn should be(true)
  }
