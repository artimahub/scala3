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

  it should "rewrite block comments before declarations as scaladoc" in {
    val content = """package test
                    |
                    |/* A method.
                    | *
                    | *  More details.
                    | */
                    |def foo(x: Int): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)

      fixResult.blocksFixed should be(1)
      fixResult.newContent shouldBe defined

      val newContent = fixResult.newContent.get
      newContent should include("/** A method.")
      newContent should include("@param x TODO FILL IN")
      newContent should include("@return TODO FILL IN")
    }
  }

  it should "not rewrite block comments before package declarations" in {
    val content = """/*
                    | * Copyright (c) Example
                    | */
                    |package test
                    |
                    |def foo(x: Int): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)

      fixResult.blocksFixed should be(0)
      fixResult.newContent shouldBe empty
    }
  }

  it should "not rewrite a license header before package scala" in {
    val content = """/* Scala (https://www.scala-lang.org)
                    | *
                    | * Copyright EPFL and Lightbend, Inc. dba Akka
                    | *
                    | * Licensed under Apache License 2.0
                    | */
                    |
                    |package scala
                    |
                    |final abstract class Foo private extends AnyVal
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      checkResult.results should have size 1
      checkResult.results.head.declaration.kind should be(DeclKind.Unknown)
      checkResult.results.head.issues shouldBe empty

      val fixResult = Fixer.fixFile(path, checkResult.results)
      fixResult.blocksFixed should be(0)
      fixResult.newContent shouldBe empty
    }
  }

  it should "not rewrite a license header before package and imports in skip-todo mode" in {
    val content = """/* Scala (https://www.scala-lang.org)
                    | *
                    | * Copyright EPFL and Lightbend, Inc. dba Akka
                    | */
                    |
                    |package scala
                    |
                    |import scala.language.`2.13`
                    |
                    |object Foo
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results, insertTodo = false)

      fixResult.blocksFixed should be(0)
      fixResult.newContent shouldBe empty
    }
  }

  it should "not rewrite a license header with generated code comment in skip-todo mode (Function10 pattern)" in {
    val content = """/*
                    | * Scala (https://www.scala-lang.org)
                    | *
                    | * Copyright EPFL and Lightbend, Inc. dba Akka
                    | *
                    | * Licensed under Apache License 2.0
                    | * (http://www.apache.org/licenses/LICENSE-2.0).
                    | *
                    | * See the NOTICE file distributed with this work for
                    | * additional information regarding copyright ownership.
                    | */
                    |
                    |// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.
                    |
                    |package scala
                    |
                    |import scala.language.`2.13`
                    |
                    |/** A function of 10 parameters. */
                    |trait Function10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends AnyRef {
                    |  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10): R
                    |}
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results, insertTodo = false)

      // Should not modify the plain /* */ license header even though Function10 trait has missing @tparam issues
      fixResult.blocksFixed should be(0)
      fixResult.newContent shouldBe empty
    }
  }

  it should "normalize plain block comments before declarations in skip-todo mode" in {
    val content = """package test
                    |
                    |/* A function doc.
                    | * More details.
                    | */
                    |def foo(x: Int): Int = x
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results, insertTodo = false)

      fixResult.blocksFixed should be(1)
      val updated = fixResult.newContent.getOrElse(fail("Expected updated content"))
      updated should include("/** A function doc.")
      updated should include("def foo(x: Int): Int = x")
      updated should not include "@param"
      updated should not include "@return"
    }
  }

  it should "not normalize inline block comments inside method bodies" in {
    val content = """package scala.concurrent.impl
                    |
                    |final class PromiseLike {
                    |  override final def toString: String =
                    |    if (true) "Future(done)"
                    |    else /* if (state.isInstanceOf[Callbacks[T]]) */ "Future(<not completed>)"
                    |
                    |  override final def value: Option[String] =
                    |    if (true) Some("done")
                    |    else /* if (state.isInstanceOf[Callbacks[T]]) */ null
                    |
                    |  override final def completeWith(other: String): this.type = {
                    |    if (other ne this) {
                    |      if (true) "done"
                    |      else /* if(state.isInstanceOf[Try[T]]) */ false
                    |    }
                    |    this
                    |  }
                    |}
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)

      val insertFix = Fixer.fixFile(path, checkResult.results)
      insertFix.newContent shouldBe empty

      val skipFix = Fixer.fixFile(path, checkResult.results, insertTodo = false)
      skipFix.newContent shouldBe empty
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
