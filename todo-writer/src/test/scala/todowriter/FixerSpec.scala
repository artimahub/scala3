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

  it should "put @see tags before @tparam, @param, @return" in {
    val text = """/** Does something.
                 | *
                 | *  @see [[Other]]
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, List("A"), List("x"), true)

    val seeIdx = result.indexOf("@see")
    val tparamIdx = result.indexOf("@tparam")
    val paramIdx = result.indexOf("@param")
    val returnIdx = result.indexOf("@return")

    seeIdx should be < tparamIdx
    seeIdx should be < paramIdx
    seeIdx should be < returnIdx
  }

  it should "preserve @see at beginning when adding new tags" in {
    // This tests the case where only @see existed and we add tparam/param/return
    val text = """/** Does something.
                 | *
                 | *  @see [[SomeClass]]
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, List("T"), List("x"), true)

    val lines = result.split("\n").map(_.trim)
    val tagLines = lines.filter(_.startsWith("*  @"))

    // @see should be first tag
    tagLines.head should include("@see")
    // @tparam should come after @see
    tagLines(1) should include("@tparam")
    // @param should come after @tparam
    tagLines(2) should include("@param")
    // @return should be last
    tagLines(3) should include("@return")
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
                 | *
                 | *  @param key the lookup key
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    block.isOneLiner should be(true)

    val decl = Declaration(DeclKind.Def, "get", Nil, List("key"), Some("Option[Int]"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should not contain Issue.MissingReturn
  }

  it should "NOT insert @return for sentence spanning multiple lines" in {
    val text = """/** Returns a two-dimensional array that contains the results of some element
                 | *  computation a number of times.
                 | *
                 | *  @param n1 the number of elements
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    block.isOneLiner should be(true)

    val decl = Declaration(DeclKind.Def, "fill", List("T"), List("n1"), Some("Array[Array[T]]"))
    val issues = ScaladocChecker.validate(block, decl)
    issues should not contain Issue.MissingReturn
  }

  it should "insert @return for multiple paragraphs" in {
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

  it should "preserve multi-line @return content" in {
    val text = """/** Creates an array.
                 | *
                 | *  @param n the size
                 | *  @return an Array of size n, where each element contains the result of computing
                 | *  `elem`.
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, List("T"), Nil, false)
    // Should preserve the multi-line @return content
    result should include("@return an Array of size n")
    result should include("`elem`.")
  }

  it should "preserve multi-line @example content" in {
    val text = """/** Shifts bits right.
                 | *
                 | *  @example {{{
                 | *  -21 >>> 3 == 536870909
                 | *  // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==
                 | *  //            00011111 11111111 11111111 11111101
                 | *  }}}
                 | */""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, List("x"), false)
    // Should preserve all lines of the @example
    result should include("@example {{{")
    result should include("-21 >>> 3 == 536870909")
    result should include("// in binary:")
    result should include("}}}")
  }
