package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

class IntegrationSpec extends AnyFlatSpec with Matchers:

  private def withTempFile(content: String)(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("todowriter-test")
    val tempFile = tempDir.resolve("Test.scala")
    try
      Files.writeString(tempFile, content)
      test(tempFile)
    finally
      Files.deleteIfExists(tempFile)
      Files.deleteIfExists(tempDir)

  "TodoWriter" should "process a file with no issues" in {
    val content = """package test
                    |
                    |/** A well-documented method.
                    | *  @param x the x value
                    | *  @return the result
                    | */
                    |def foo(x: Int): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(false)
    }
  }

  it should "detect missing params" in {
    val content = """package test
                    |
                    |/** A method with missing param docs.
                    | */
                    |def foo(x: Int, y: String): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(true)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingParam(List("x", "y")))
    }
  }

  it should "detect missing tparams" in {
    val content = """package test
                    |
                    |/** A generic method.
                    | */
                    |def foo[A, B](x: A): B = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(true)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingTparam(List("A", "B")))
    }
  }

  it should "detect missing return" in {
    val content = """package test
                    |
                    |/** A method that returns something.
                    | *
                    | *  This is a longer description.
                    | */
                    |def foo(): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(true)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingReturn)
    }
  }

  it should "not require @return for one-liner" in {
    val content = """package test
                    |
                    |/** Returns the count. */
                    |def count: Int = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      val issues = result.results.flatMap(_.issues)
      issues should not contain Issue.MissingReturn
    }
  }

  it should "apply fixes correctly" in {
    // Note: "A method." is a one-liner, so @return should NOT be added
    val content = """package test
                    |
                    |/** A method.
                    | */
                    |def foo(x: Int): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)

      fixResult.blocksFixed should be(1)
      fixResult.newContent shouldBe defined

      val newContent = fixResult.newContent.get
      newContent should include("@param x TODO FILL IN")
      // @return should NOT be added because it's a one-liner
      newContent should not include "@return TODO FILL IN"
    }
  }

  it should "apply @return fix for multi-line content" in {
    val content = """package test
                    |
                    |/** A method.
                    | *
                    | *  This has more details.
                    | */
                    |def foo(x: Int): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)

      fixResult.blocksFixed should be(1)
      fixResult.newContent shouldBe defined

      val newContent = fixResult.newContent.get
      newContent should include("@param x TODO FILL IN")
      newContent should include("@return TODO FILL IN")
    }
  }

  it should "insert missing @param for polymorphic function-typed parameter" in {
    val content = """package test
                    |
                    |/** The named tuple consisting of all element values of this tuple mapped by
                    | *  the polymorphic mapping function `f`.
                    | */
                    |inline def map[F[_]](f: [t] => t => F[t]): Map[String, F] = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val issues = checkResult.results.flatMap(_.issues)
      issues should contain(Issue.MissingParam(List("f")))

      val fixResult = Fixer.fixFile(path, checkResult.results)
      fixResult.newContent shouldBe defined
      fixResult.newContent.get should include("@param f TODO FILL IN")
    }
  }

  it should "validate class parameters" in {
    val content = """package test
                    |
                    |/** A class.
                    | */
                    |class Foo[A](x: Int, y: String)
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(true)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingTparam(List("A")))
      issues should contain(Issue.MissingParam(List("x", "y")))
    }
  }

  it should "validate trait type parameters" in {
    val content = """package test
                    |
                    |/** A trait.
                    | */
                    |trait Foo[A, B]
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(true)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingTparam(List("A", "B")))
    }
  }

  it should "handle multiple declarations in one file" in {
    val content = """package test
                    |
                    |/** First method. */
                    |def one(x: Int): Unit = ()
                    |
                    |/** Second method.
                    | *
                    | *  With more description.
                    | */
                    |def two(y: String): Int = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.results should have size 2

      // First method: one-liner, has param
      val firstIssues = result.results(0).issues
      firstIssues should contain(Issue.MissingParam(List("x")))
      firstIssues should not contain Issue.MissingReturn // Unit return

      // Second method: multi-line, has param, needs return
      val secondIssues = result.results(1).issues
      secondIssues should contain(Issue.MissingParam(List("y")))
      secondIssues should contain(Issue.MissingReturn) // Not one-liner
    }
  }

  it should "preserve proper indentation when fixing" in {
    val content = """package test
                    |
                    |object Foo {
                    |  /** A method. */
                    |  def bar(x: Int): String = ???
                    |}
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)

      fixResult.newContent shouldBe defined
      val newContent = fixResult.newContent.get

      // Check that the fixed scaladoc maintains proper indentation
      val lines = newContent.split("\n")
      val scaladocLines = lines.filter(l => l.contains("*") && !l.contains("def"))
      scaladocLines.foreach { line =>
        // Lines should start with proper indentation (2 spaces for object body)
        if line.trim.startsWith("*") then
          line should startWith("   *") // 2 spaces + space + *
      }
    }
  }

  it should "skip unnamed using parameters when inserting missing param tags" in {
    val content = """package test
                    |
                    |/** A method.
                    | *
                    | *  More details.
                    | */
                    |def foo(x: Int)(using Context): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)

      fixResult.blocksFixed should be(1)
      fixResult.newContent shouldBe defined

      val newContent = fixResult.newContent.get
      newContent should include("@param x TODO FILL IN")
      newContent should not include "@param Context TODO FILL IN"
      newContent should include("@return TODO FILL IN")
    }
  }

  it should "insert missing @param for annotated parameters" in {
    val content = """package test
                    |
                    |object Foo:
                    |  /** Local identity method. */
                    |  @inline def locally[T](@DeprecatedName("x") x: T): T = x
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)

      fixResult.blocksFixed should be(1)
      fixResult.newContent shouldBe defined

      val newContent = fixResult.newContent.get
      newContent should include("@param x TODO FILL IN")
    }
  }

  it should "not add duplicate @param when existing tag uses visibility modifiers" in {
    val content = """package test
                    |
                    |/** A class.
                    | *  @param private[immutable] val len1 the number of elements in prefix1
                    | */
                    |class Foo(private[immutable] val len1: Int)
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      checkResult.hasIssues should be(false)

      val fixResult = Fixer.fixFile(path, checkResult.results)
      fixResult.blocksFixed should be(0)
      fixResult.newContent shouldBe None
    }
  }

  it should "accept backticked @param name for backticked parameter" in {
    val content = """package test
                    |
                    |/** A method.
                    | *  @param `type` the input value
                    | *  @return the output value
                    | */
                    |def foo(`type`: Int): Int = `type`
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      checkResult.hasIssues should be(false)

      val fixResult = Fixer.fixFile(path, checkResult.results)
      fixResult.blocksFixed should be(0)
      fixResult.newContent shouldBe None
    }
  }

  it should "not add duplicate @param when existing tag uses protected qualifier" in {
    val content = """package test
                    |
                    |/** A class.
                    | *  @param protected[collection] val len1 the number of elements in prefix1
                    | */
                    |class Foo(protected[collection] val len1: Int)
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      checkResult.hasIssues should be(false)

      val fixResult = Fixer.fixFile(path, checkResult.results)
      fixResult.blocksFixed should be(0)
      fixResult.newContent shouldBe None
    }
  }

  it should "not treat @return mention in prose as return tag" in {
    val content = """package test
                    |
                    |/** A method.
                    | *
                    | *  See @return docs in another method.
                    | */
                    |def foo(x: Int): Int = x
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val issues = checkResult.results.flatMap(_.issues)
      issues should contain(Issue.MissingParam(List("x")))
      issues should contain(Issue.MissingReturn)
    }
  }

  it should "not treat @param mention in continuation text as a new param tag" in {
    val content = """package test
                    |
                    |/** A method.
                    | *  @param x the x value
                    | *    mention @param y in prose
                    | *  @return the output value
                    | */
                    |def foo(x: Int): Int = x
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val issues = checkResult.results.flatMap(_.issues)
      issues should be(empty)
    }
  }

  it should "accept @param for erased parameters" in {
    val content = """package test
                    |
                    |/** A method.
                    | *  @param x the input value
                    | *  @return the output value
                    | */
                    |def foo(erased x: Int): Int = 0
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      checkResult.hasIssues should be(false)
    }
  }
