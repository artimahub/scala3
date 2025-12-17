package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FixerSpec extends AnyFlatSpec with Matchers:

  "Fixer.buildFixedBlock" should "insert missing @param tag with TODO FILL IN" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    result should include("@param x TODO FILL IN")
  }

  it should "insert missing @tparam tag with TODO FILL IN" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, List("A"), Nil, false)
    result should include("@tparam A TODO FILL IN")
  }

  it should "insert missing @return tag with TODO FILL IN" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, Nil, true)
    result should include("@return TODO FILL IN")
  }

  it should "preserve existing content when inserting" in {
    val text = """/** Does something important.
                 | *
                 | *  @param x existing param
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("y"), false)
    result should include("Does something important")
    result should include("@param x existing param")
    result should include("@param y TODO FILL IN")
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

  it should "sort existing tags to proper order (tparam, param, return)" in {
    // Original has @param before @tparam - should be reordered
    val text = """/** Does something.
                 | *
                 | *  @param x existing
                 | *  @tparam A existing
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("y"), false)
    val tparamIdx = result.indexOf("@tparam")
    val paramXIdx = result.indexOf("@param x")
    val paramYIdx = result.indexOf("@param y")
    tparamIdx should be < paramXIdx
    tparamIdx should be < paramYIdx
  }

  it should "not have blank lines between tags" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, List("A", "B"), List("x", "y"), true)
    val lines = result.split("\n")

    // Find the first tag line index
    val firstTagIdx = lines.indexWhere(l => l.contains("@tparam") || l.contains("@param") || l.contains("@return"))
    // Find the last tag line index (before closing */)
    val closingIdx = lines.indexWhere(_.trim == "*/")

    // All lines between first tag and closing should be tags (no blank lines)
    val tagSection = lines.slice(firstTagIdx, closingIdx)
    tagSection.foreach { line =>
      val trimmed = line.trim
      (trimmed.isEmpty || trimmed == "*") should be(false)
    }
  }

  it should "have all tags in order: @tparam, @param, @return" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, List("A", "B"), List("x", "y"), true)

    val tparamAIdx = result.indexOf("@tparam A")
    val tparamBIdx = result.indexOf("@tparam B")
    val paramXIdx = result.indexOf("@param x")
    val paramYIdx = result.indexOf("@param y")
    val returnIdx = result.indexOf("@return")

    // All tparams before all params before return
    tparamAIdx should be < paramXIdx
    tparamBIdx should be < paramXIdx
    paramXIdx should be < returnIdx
    paramYIdx should be < returnIdx
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
    newText should include("@param x TODO FILL IN")
    newText should include("@return TODO FILL IN")
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

  "Fixer" should "keep initial text on same line as /**" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    result should startWith("/** Does something.")
  }

  it should "move misplaced initial text to /** line" in {
    val text = """/**
                 | * Does something.
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    result should startWith("/** Does something.")
    // Should not have a blank line at the start
    result should not include "/**\n *\n"
  }

  it should "add blank line before tags if missing" in {
    val text = "/** Does something. */"
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    // Should have a blank line before @param
    val lines = result.split("\n")
    val paramLineIdx = lines.indexWhere(_.contains("@param"))
    if paramLineIdx > 0 then
      val prevLine = lines(paramLineIdx - 1).trim
      prevLine should be("*")
  }

  it should "not duplicate blank line before tags if already present" in {
    val text = """/** Does something.
                 | *
                 | *  @param y existing
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    // Count blank star lines
    val blankStarLines = result.split("\n").count(l => l.trim == "*")
    blankStarLines should be(1)
  }

  it should "NOT insert @return for one-liner scaladoc" in {
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
