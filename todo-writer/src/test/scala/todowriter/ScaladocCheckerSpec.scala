package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScaladocCheckerSpec extends AnyFlatSpec with Matchers:

  private def makeBlock(
      params: List[String] = Nil,
      tparams: List[String] = Nil,
      hasReturn: Boolean = false,
      isOneLiner: Boolean = false
  ): ScaladocBlock =
    ScaladocBlock("", 0, 0, 1, params, tparams, hasReturn, isOneLiner)

  private def makeDecl(
      kind: DeclKind = DeclKind.Def,
      params: List[String] = Nil,
      tparams: List[String] = Nil,
      returnType: Option[String] = None
  ): Declaration =
    Declaration(kind, "test", tparams, params, returnType)

  "ScaladocChecker.validate" should "detect missing @param" in {
    val block = makeBlock()
    val decl = makeDecl(params = List("x", "y"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.MissingParam(List("x", "y")))
  }

  it should "detect extra @param" in {
    val block = makeBlock(params = List("x", "z"))
    val decl = makeDecl(params = List("x"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.UnknownParam(List("z")))
  }

  it should "detect missing @tparam" in {
    val block = makeBlock()
    val decl = makeDecl(tparams = List("A", "B"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.MissingTparam(List("A", "B")))
  }

  it should "detect extra @tparam" in {
    val block = makeBlock(tparams = List("A", "C"))
    val decl = makeDecl(tparams = List("A"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.UnknownTparam(List("C")))
  }

  it should "detect missing @return for non-Unit" in {
    val block = makeBlock(isOneLiner = false)
    val decl = makeDecl(returnType = Some("String"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.MissingReturn)
  }

  it should "detect unnecessary @return for Unit" in {
    val block = makeBlock(hasReturn = true)
    val decl = makeDecl(returnType = Some("Unit"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.UnnecessaryReturn)
  }

  it should "pass when all tags present" in {
    val block = makeBlock(
      params = List("x"),
      tparams = List("A"),
      hasReturn = true,
      isOneLiner = false
    )
    val decl = makeDecl(
      params = List("x"),
      tparams = List("A"),
      returnType = Some("String")
    )
    val issues = ScaladocChecker.validate(block, decl)
    issues should be(empty)
  }

  it should "ignore @return for class" in {
    val block = makeBlock()
    val decl = makeDecl(kind = DeclKind.Class, returnType = Some("String"))
    val issues = ScaladocChecker.validate(block, decl)
    issues.collect { case Issue.MissingReturn => Issue.MissingReturn } should be(empty)
  }

  it should "ignore @return for trait" in {
    val block = makeBlock()
    val decl = makeDecl(kind = DeclKind.Trait, returnType = Some("String"))
    val issues = ScaladocChecker.validate(block, decl)
    issues.collect { case Issue.MissingReturn => Issue.MissingReturn } should be(empty)
  }

  it should "NOT require @return for one-liner scaladoc" in {
    val block = makeBlock(isOneLiner = true)
    val decl = makeDecl(returnType = Some("String"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should not contain Issue.MissingReturn
  }

  it should "validate @param for class" in {
    val block = makeBlock()
    val decl = makeDecl(kind = DeclKind.Class, params = List("x"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.MissingParam(List("x")))
  }

  it should "validate @tparam for class" in {
    val block = makeBlock()
    val decl = makeDecl(kind = DeclKind.Class, tparams = List("A"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.MissingTparam(List("A")))
  }

  it should "validate @param for trait" in {
    val block = makeBlock()
    val decl = makeDecl(kind = DeclKind.Trait, params = List("x"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.MissingParam(List("x")))
  }

  it should "validate @tparam for trait" in {
    val block = makeBlock()
    val decl = makeDecl(kind = DeclKind.Trait, tparams = List("A"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.MissingTparam(List("A")))
  }

  it should "not validate @param/@tparam for object" in {
    val block = makeBlock()
    val decl = makeDecl(kind = DeclKind.Object)
    val issues = ScaladocChecker.validate(block, decl)
    issues should be(empty)
  }

  it should "not validate @param/@tparam for val" in {
    val block = makeBlock()
    val decl = makeDecl(kind = DeclKind.Val)
    val issues = ScaladocChecker.validate(block, decl)
    issues should be(empty)
  }
