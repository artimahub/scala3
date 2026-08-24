package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Regression tests for the placement of newly inserted Scaladoc stubs: the
 *  `/** TODO FILL IN */` stub must be inserted ABOVE the declaration's leading
 *  annotation/comment lines, not between them and the declaration (otherwise
 *  the annotation ends up sitting between the doc comment and the member).
 */
class ScaladocStubPlacementSpec extends AnyFlatSpec with Matchers:

  private def withTempFile(content: String)(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("todowriter-stub-placement")
    val tempFile = tempDir.resolve("Test.scala")
    try
      Files.writeString(tempFile, content)
      test(tempFile)
    finally
      Files.deleteIfExists(tempFile)
      Files.deleteIfExists(tempDir)

  private def fixedContent(content: String): String =
    var result = ""
    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)
      fixResult.newContent shouldBe defined
      result = fixResult.newContent.get
    }
    result

  /** Assert that `stub` appears immediately above the `@deprecated` line (i.e.
   *  there is no annotation line between the stub and the declaration, and the
   *  stub is not below the annotation).
   */
  private def assertStubAboveAnnotation(newContent: String, annotation: String): Unit =
    val stubIdx = newContent.indexOf("/** TODO FILL IN */")
    val annotationIdx = newContent.indexOf(annotation)
    stubIdx should be >= 0
    annotationIdx should be >= 0
    // the stub must come before the annotation
    stubIdx should be < annotationIdx
    // and the annotation must not sit between the stub and the declaration
    val declarationIdx = newContent.indexOf("val groupNames")
    declarationIdx should be >= 0
    val between = newContent.substring(stubIdx + "/** TODO FILL IN */".length, declarationIdx)
    between.trim should be(annotation)

  it should "place the TODO stub above a single-line @deprecated annotation" in {
    // Mirrors library/src/scala/util/matching/Regex.scala: the deprecated val has
    // no Scaladoc at all, and the annotation sits on its own line above it.
    val content = """package test
                    |
                    |/** A match object. */
                    |class Match {
                    |  @deprecated("groupNames does not include inline group names, and should not be used anymore", "2.13.7")
                    |  val groupNames: Seq[String] = Nil
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)
    newContent should include(
      "  /** TODO FILL IN */\n" +
        "  @deprecated(\"groupNames does not include inline group names, and should not be used anymore\", \"2.13.7\")\n" +
        "  val groupNames: Seq[String] = Nil")
  }

  it should "place the TODO stub above an @deprecated annotation on an undocumented class" in {
    val content = """package test
                    |
                    |@deprecated("gone", "2.13.0")
                    |class Bar(x: Int)
                    |""".stripMargin

    val newContent = fixedContent(content)
    // `Bar` has a constructor param, so the stub is multi-line.
    newContent should include(
      " *  @param x TODO FILL IN\n" +
        " */\n" +
        "@deprecated(\"gone\", \"2.13.0\")\n" +
        "class Bar(x: Int)")
    // the stub must be directly above the annotation
    val stubIdx = newContent.indexOf("/** TODO FILL IN")
    val annotationIdx = newContent.indexOf("@deprecated")
    stubIdx should be >= 0
    stubIdx should be < annotationIdx
  }

  it should "place the TODO stub above multiple stacked annotations" in {
    val content = """package test
                    |
                    |@deprecatedInheritance("Scheduled for being final", "2.13.0")
                    |@deprecated("As of JDK 17, 'strictfp' is not required", "3.8.0")
                    |class Foo extends scala.annotation.StaticAnnotation
                    |""".stripMargin

    val newContent = fixedContent(content)
    newContent should include(
      "/** TODO FILL IN */\n" +
        "@deprecatedInheritance(\"Scheduled for being final\", \"2.13.0\")\n" +
        "@deprecated(\"As of JDK 17, 'strictfp' is not required\", \"3.8.0\")\n" +
        "class Foo extends scala.annotation.StaticAnnotation")
  }

  it should "place the TODO stub above a multi-line annotation" in {
    val content = """package test
                    |
                    |class Match {
                    |  @deprecated("groupNames does not include inline group names, and should not be used anymore" +
                    |    " if using a type with a consistent order", "2.13.7")
                    |  val groupNames: Seq[String] = Nil
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)
    newContent should include(
      "  /** TODO FILL IN */\n" +
        "  @deprecated(\"groupNames does not include inline group names, and should not be used anymore\" +\n" +
        "    \" if using a type with a consistent order\", \"2.13.7\")\n" +
        "  val groupNames: Seq[String] = Nil")
  }

  it should "keep the stub directly above the line when the annotation shares the declaration line" in {
    // The whole `@inline override def ...` declaration is on one line, so the
    // stub is inserted directly above that line. `newArray` has a param and a
    // non-Unit return, so the stub is multi-line.
    val content = """package test
                    |
                    |final private class ByteManifest extends Base {
                    |  @inline override def newArray(len: Int): Array[Byte] = new Array[Byte](len)
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)
    newContent should include(
      "  @param len TODO FILL IN\n" +
        "   *  @return TODO FILL IN\n" +
        "   */\n" +
        "  @inline override def newArray(len: Int): Array[Byte] = new Array[Byte](len)")
    // the stub must be directly above the declaration line, not between the
    // `@inline` annotation and the `def` keyword
    val stubIdx = newContent.indexOf("/** TODO FILL IN")
    val declIdx = newContent.indexOf("@inline override def newArray")
    stubIdx should be >= 0
    stubIdx should be < declIdx
  }

  it should "place the TODO stub above a // comment preceding the declaration" in {
    val content = """package test
                    |
                    |class Match {
                    |  // group names do not include inline group names
                    |  val groupNames: Seq[String] = Nil
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)
    newContent should include(
      "  /** TODO FILL IN */\n" +
        "  // group names do not include inline group names\n" +
        "  val groupNames: Seq[String] = Nil")
  }

  it should "not treat the tail of a multi-line statement as an annotation preamble" in {
    // The line directly above `val x` is the closing of a multi-line `val
    // previous` statement. The stub for `val x` must stay directly above the
    // declaration and must not jump above the statement.
    val content = """package test
                    |
                    |class Foo {
                    |  /** The previous value. */
                    |  val previous = compute(
                    |    arg1, arg2)
                    |  val x = 1
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)
    newContent should include(
      "    arg1, arg2)\n" +
        "  /** TODO FILL IN */\n" +
        "  val x = 1")
    // the stub for `val x` must stay directly above the declaration; it must
    // not be moved above the multi-line `val previous` statement
    val stubIdx = newContent.lastIndexOf("/** TODO FILL IN */")
    val xIdx = newContent.indexOf("val x = 1")
    val previousIdx = newContent.indexOf("val previous")
    stubIdx should be >= 0
    stubIdx should be < xIdx
    stubIdx should be > previousIdx
  }

  it should "be idempotent: re-checking a fixed file reports the declaration as documented" in {
    val content = """package test
                    |
                    |/** A match object. */
                    |class Match {
                    |  @deprecated("groupNames does not include inline group names, and should not be used anymore", "2.13.7")
                    |  val groupNames: Seq[String] = Nil
                    |}
                    |""".stripMargin

    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)
      fixResult.newContent shouldBe defined
      Fixer.writeFixedFile(path, fixResult.newContent.get)

      // After the fix, the stub sits above the annotation and documents the
      // declaration, so no synthetic result should be produced for it anymore.
      val recheck = ScaladocChecker.checkFile(path)
      val syntheticNames = recheck.results.filter(_.scaladoc.synthetic).map(_.declaration.name)
      syntheticNames should not contain "groupNames"
    }
  }
