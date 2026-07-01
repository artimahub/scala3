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

  it should "detect missing return when method has @param" in {
    val content = """package test
                    |
                    |/** A method that returns something. */
                    |def foo(x: Int): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(true)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingReturn)
    }
  }

  it should "not require @return when method has no @param/@tparam/@throws" in {
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
      // @return is added because there is a @param
      newContent should include("@return TODO FILL IN")
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

  it should "detect missing tparams for transparent traits with higher-kinded and F-bounded params" in {
    val content = """package test
                    |
                    |/** Base trait for immutable set operations.
                    | */
                    |transparent trait SetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
                    |  extends collection.SetOps[A, CC, C]
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingTparam(List("A", "CC", "C")))

      val fixResult = Fixer.fixFile(path, result.results)
      fixResult.newContent shouldBe defined
      val newContent = fixResult.newContent.get
      newContent should include("@tparam A TODO FILL IN")
      newContent should include("@tparam CC TODO FILL IN")
      newContent should include("@tparam C TODO FILL IN")
    }
  }

  it should "insert only the missing trailing tparam for SetOps-like scaladoc" in {
    val content = """package test
                    |
                    |/** Base trait for immutable set operations
                    | *
                    | *  @tparam A the element type of the set
                    | *  @tparam CC the type constructor for the resulting set
                    | */
                    |transparent trait SetOps[A, +CC[X], +C <: SetOps[A, CC, C]]
                    |  extends collection.SetOps[A, CC, C]
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingTparam(List("C")))

      val fixResult = Fixer.fixFile(path, result.results)
      fixResult.newContent shouldBe defined
      val newContent = fixResult.newContent.get
      newContent should include("@tparam A the element type of the set")
      newContent should include("@tparam CC the type constructor for the resulting set")
      newContent should include("@tparam C TODO FILL IN")
    }
  }

  it should "detect missing tparams for defs with higher-kinded type param bounds" in {
    val content = """package test
                    |
                    |object Ordering:
                    |  trait ExtraImplicits {
                    |    /** Not in the standard scope due to the potential for divergence.
                    |     */
                    |    implicit def seqOrdering[CC[X] <: scala.collection.Seq[X], T](implicit ord: Ordering[T]): Ordering[CC[T]] = ???
                    |  }
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingTparam(List("CC", "T")))

      val fixResult = Fixer.fixFile(path, result.results)
      fixResult.newContent shouldBe defined
      val newContent = fixResult.newContent.get
      newContent should include("@tparam CC TODO FILL IN")
      newContent should include("@tparam T TODO FILL IN")
    }
  }

  it should "insert only the missing trailing tparam for seqOrdering-like scaladoc" in {
    val content = """package test
                    |
                    |object Ordering:
                    |  trait ExtraImplicits {
                    |    /** Not in the standard scope due to the potential for divergence:
                    |     *  For instance `implicitly[Ordering[Any]]` diverges in its presence.
                    |     *
                    |     *  @tparam CC the higher-kinded type constructor for the sequence type, bounded by `scala.collection.Seq`
                    |     */
                    |    implicit def seqOrdering[CC[X] <: scala.collection.Seq[X], T](implicit ord: Ordering[T]): Ordering[CC[T]] = ???
                    |  }
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingTparam(List("T")))
      issues should contain(Issue.MissingParam(List("ord")))

      val fixResult = Fixer.fixFile(path, result.results)
      fixResult.newContent shouldBe defined
      val newContent = fixResult.newContent.get
      newContent should include("@tparam CC the higher-kinded type constructor for the sequence type, bounded by `scala.collection.Seq`")
      newContent should include("@tparam T TODO FILL IN")
      newContent should include("@param ord TODO FILL IN")
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

      // First method: has @param x but Unit return, so no @return
      val firstIssues = result.results(0).issues
      firstIssues should contain(Issue.MissingParam(List("x")))
      firstIssues should not contain Issue.MissingReturn

      // Second method: has @param y and non-Unit return, so @return is added
      val secondIssues = result.results(1).issues
      secondIssues should contain(Issue.MissingParam(List("y")))
      secondIssues should contain(Issue.MissingReturn)
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

  it should "detect missing @param for private scoped class constructor params" in {
    val content = """package test
                    |
                    |/** Stepper for object arrays.
                    | */
                    |private[collection] class ObjectArrayStepper[A <: Object | Null](underlying: Array[A], _i0: Int, _iN: Int)
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(true)
      val issues = result.results.flatMap(_.issues)
      issues should contain(Issue.MissingParam(List("underlying", "_i0", "_iN")))
    }
  }

  it should "not synthesize a duplicate stub when the doc comment sits before an annotation" in {
    // The class already has a Scaladoc comment, but it is placed BEFORE the
    // @deprecated annotation rather than immediately above the declaration.
    // It must still be recognized as documented -- no synthetic stub.
    val content = """package test
                    |
                    |/** A documented class. */
                    |@deprecated("gone", "2.13.0")
                    |class Foo(x: Int)
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.results.count(_.scaladoc.synthetic) should be(0)
    }
  }

  it should "skip multiple annotations, including ones with quoted/bracketed args, before a documented declaration" in {
    // The pre-existing comment sits before two annotations, one carrying a
    // string argument that contains quotes and commas. Both annotation lines
    // must be skipped so the class is still recognized as documented.
    val content = """package test
                    |
                    |/** A documented class. */
                    |@deprecatedInheritance("Scheduled for being final", "2.13.0")
                    |@deprecated("As of JDK 17, 'strictfp' is not required", "3.8.0")
                    |class Foo extends scala.annotation.StaticAnnotation
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.results.count(_.scaladoc.synthetic) should be(0)
    }
  }

  it should "treat a declaration as documented when line comments sit between its comment and it" in {
    // The doc comment is separated from the class by annotations AND `//` line
    // comments (a commented-out @deprecated). It must still be recognized as
    // documented -- no duplicate stub.
    val content = """package test
                    |
                    |/** A documented trait. */
                    |@nowarn("cat=deprecation")
                    |// TODO undeprecated for now
                    |// @deprecated("gone", "2.10.0")
                    |trait Foo[T]
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.results.count(_.scaladoc.synthetic) should be(0)
    }
  }

  it should "still synthesize docs for an undocumented annotated class" in {
    // No comment anywhere: the annotated class is genuinely undocumented.
    val content = """package test
                    |
                    |@deprecated("gone", "2.13.0")
                    |class Bar(x: Int)
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      val synth = result.results.filter(_.scaladoc.synthetic).map(_.declaration.name)
      synth should contain("Bar")
    }
  }

  it should "not synthesize docs for a local def nested inside a method body" in {
    // `loop` is a local helper inside `replace`; it is not API and must not be
    // flagged. `replace` itself (a member) still is.
    val content = """package test
                    |
                    |object Escapes {
                    |  def replace(str: String, first: Int): String = {
                    |    def loop(i: Int, next: Int): String = {
                    |      if (next >= 0) loop(i, next - 1) else str
                    |    }
                    |    loop(0, first)
                    |  }
                    |}
                    |""".stripMargin

    withTempFile(content) { path =>
      val synth = ScaladocChecker.checkFile(path).results.filter(_.scaladoc.synthetic).map(_.declaration.name)
      synth should contain("replace")     // member -> flagged
      synth should not contain "loop"      // local def -> skipped
    }
  }

  it should "not synthesize docs for an undocumented private-scoped class" in {
    // ObjectArrayStepper has NO Scaladoc at all, but it is package-private, so it
    // is not part of the documented (Scaladoc) API surface and should be skipped.
    val content =
      """private[collection] class ObjectArrayStepper[A <: Object | Null](underlying: Array[A], _i0: Int, _iN: Int)
        |  extends SomeBase[A](_i0, _iN)
        |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      result.hasIssues should be(false)
      result.results.count(_.scaladoc.synthetic) should be(0)
    }
  }

  it should "not synthesize docs for undocumented private methods but still flag public ones" in {
    val content =
      """package test
        |
        |private def spaces(n: Int) = " ".repeat(n)
        |
        |def withReset(f: Int => Int): Int = f(0)
        |
        |protected def visible(x: Int): Int = x
        |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      val synthNames = result.results.filter(_.scaladoc.synthetic).map(_.declaration.name)
      synthNames should not contain "spaces"     // private -> skipped
      synthNames should contain("withReset")      // public -> flagged
      synthNames should contain("visible")        // protected -> still flagged
    }
  }
  it should "synthesize a description-only stub for an undocumented no-arg method" in {
    // `reader` has no params, no type params, and an inferred return type, so it
    // has no missing tags. It is still public and undocumented, so it should get
    // a description-only Scaladoc stub.
    val content = """package test
                    |
                    |class Source {
                    |  def reader() = new java.io.InputStreamReader(in)
                    |}
                    |""".stripMargin

    withTempFile(content) { path =>
      val result = ScaladocChecker.checkFile(path)
      val readerResult = result.results.find(r => r.scaladoc.synthetic && r.declaration.name == "reader")
      readerResult.map(_.issues) should be(Some(List(Issue.MissingDescription)))

      val fixResult = Fixer.fixFile(path, result.results)
      fixResult.newContent.get should include("/** TODO FILL IN */")
      fixResult.newContent.get should include("def reader()")
    }
  }

  it should "skip undocumented declarations when skipUndocumented = true" in {
    // One method has a Scaladoc block missing @param; another method has no Scaladoc at all.
    // With skipUndocumented = true, only the documented-but-missing-tag method should be reported.
    val content = """package test
                    |
                    |/** A documented method missing @param. */
                    |def documented(x: Int): String = ???
                    |
                    |def undocumented(y: Int): String = ???
                    |""".stripMargin

    withTempFile(content) { path =>
      val withUndoc = ScaladocChecker.checkFile(path)
      withUndoc.results.flatMap(_.issues) should contain allOf (
        Issue.MissingParam(List("x")),
        Issue.MissingParam(List("y")),
        Issue.MissingReturn
      )

      val withoutUndoc = ScaladocChecker.checkFile(path, skipUndocumented = true)
      val issues = withoutUndoc.results.flatMap(_.issues)
      issues should contain(Issue.MissingParam(List("x")))
      issues should not contain Issue.MissingParam(List("y"))
      // The @return tag on the undocumented method also should not be reported.
      withoutUndoc.results.count(_.scaladoc.synthetic) should be(0)
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

  it should "detect duplicate @param tags" in {
    val content = """package test
                    |
                    |/** A method.
                    | *  @param x the x value
                    | *  @param x the x value again
                    | */
                    |def foo(x: Int): Int = x
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val issues = checkResult.results.flatMap(_.issues)
      issues should contain(Issue.UnknownParam(List("x")))
    }
  }

  it should "handle unknown tags without confusing signature parsing" in {
    val content = """package test
                    |
                    |/** A method.
                    | *  @param x the x value
                    | *  @see [[SomeClass]]
                    | *  @example code here
                    | *  @return the result
                    | */
                    |def foo(x: Int): Int = x
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      checkResult.hasIssues should be(false)
    }
  }
