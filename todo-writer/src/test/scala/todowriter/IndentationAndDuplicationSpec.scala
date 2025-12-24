package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IndentationAndDuplicationSpec extends AnyFlatSpec with Matchers:

  "Fixer.applyFixes" should "replace a single-line indented scaladoc with multi-line keeping indentation and not duplicating the declaration" in {
    val text =
      """  /** Provides an implicit conversion from the Array object to a collection Factory. */
        |  implicit def toFactory[A : ClassTag](dummy: Array.type): Factory[A, Array[A]] = new ArrayFactory(dummy)""".stripMargin

    val block = ScaladocBlock.findAll(text).head
    val chunk = Declaration.getDeclarationAfter(text, block.endIndex)
    val decl = Declaration.parse(chunk)
    val result = CheckResult(block, decl, List(Issue.MissingTparam(List("A")), Issue.MissingParam(List("dummy"))))

    val (newText, _) = Fixer.applyFixes(text, List(result))

    val expected =
      """  /** Provides an implicit conversion from the Array object to a collection Factory.
        |   *
        |   *  @tparam A TODO FILL IN
        |   *  @param dummy TODO FILL IN
        |   */
        |  implicit def toFactory[A : ClassTag](dummy: Array.type): Factory[A, Array[A]] = new ArrayFactory(dummy)""".stripMargin

    newText should be(expected)
  }