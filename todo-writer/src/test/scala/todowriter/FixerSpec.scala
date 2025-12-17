package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FixerSpec extends AnyFlatSpec with Matchers:

  "Fixer.buildFixedBlock" should "insert missing @param tag" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    result should include("@param x TODO")
  }

  it should "insert missing @tparam tag" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, List("A"), Nil, false)
    result should include("@tparam A TODO")
  }

  it should "insert missing @return tag" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, Nil, true)
    result should include("@return TODO")
  }

  it should "preserve existing content when inserting" in {
    val text = """/** Does something important.
                 | *  @param x existing param
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("y"), false)
    result should include("Does something important")
    result should include("@param x existing param")
    result should include("@param y TODO")
  }

  it should "convert single-line to multi-line when adding tags" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    result should include("\n")
    result should startWith("/**")
    result should endWith("*/")
  }

  it should "align * with first * of /**" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    // The * should be at column 1 (space then *)
    val lines = result.split("\n")
    lines.drop(1).dropRight(1).foreach { line =>
      line should startWith(" *")
    }
  }

  it should "handle indented scaladoc" in {
    val text = "    /** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    val lines = result.split("\n")
    // Continuation lines should have 4 spaces before *
    lines.drop(1).dropRight(1).foreach { line =>
      line should startWith("     *")
    }
  }

  it should "insert @tparam before @param" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, List("A"), List("x"), false)
    val tparamIdx = result.indexOf("@tparam")
    val paramIdx = result.indexOf("@param")
    tparamIdx should be < paramIdx
  }

  it should "insert @return after @param" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), true)
    val paramIdx = result.indexOf("@param")
    val returnIdx = result.indexOf("@return")
    paramIdx should be < returnIdx
  }

  "Fixer.applyFixes" should "apply multiple fixes" in {
    val text = """/** Method one. */
                 |def one(x: Int): String = ???
                 |
                 |/** Method two. */
                 |def two(y: Int): String = ???""".stripMargin

    val blocks = ScaladocBlock.findAll(text)
    val results = blocks.map { block =>
      val chunk = Declaration.getDeclarationAfter(text, block.endIndex)
      val decl = Declaration.parse(chunk)
      CheckResult(block, decl, List(Issue.MissingParam(List("x")), Issue.MissingReturn))
    }

    val (newText, count) = Fixer.applyFixes(text, results)
    count should be(2)
    newText should include("@param x TODO")
    newText should include("@return TODO")
  }

  it should "not modify text when no fixes needed" in {
    val text = """/** Method one.
                 | *  @param x the param
                 | *  @return the result
                 | */
                 |def one(x: Int): String = ???""".stripMargin

    val blocks = ScaladocBlock.findAll(text)
    val results = blocks.map { block =>
      val chunk = Declaration.getDeclarationAfter(text, block.endIndex)
      val decl = Declaration.parse(chunk)
      CheckResult(block, decl, Nil) // No issues
    }

    val (newText, count) = Fixer.applyFixes(text, results)
    count should be(0)
    newText should be(text)
  }

  "Fixer" should "NOT insert @return for one-liner scaladoc" in {
    val text = "/** Returns the count. */"
    val block = ScaladocBlock.findAll(text).head
    block.isOneLiner should be(true)

    // If this is a one-liner, the checker shouldn't produce MissingReturn
    // So we verify that the validation logic is correct
    val decl = Declaration(DeclKind.Def, "count", Nil, Nil, Some("Int"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should not contain Issue.MissingReturn
  }

  it should "NOT insert @return for one-liner with @param tags" in {
    val text = """/** Gets the value for the given key.
                 | *  @param key the lookup key
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    block.isOneLiner should be(true)

    val decl = Declaration(DeclKind.Def, "get", Nil, List("key"), Some("Option[Int]"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should not contain Issue.MissingReturn
  }

  it should "insert @return for multi-line descriptive content" in {
    val text = """/** Computes the result.
                 | *
                 | *  This method performs complex calculation.
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    block.isOneLiner should be(false)

    val decl = Declaration(DeclKind.Def, "compute", Nil, Nil, Some("Int"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should contain(Issue.MissingReturn)
  }
