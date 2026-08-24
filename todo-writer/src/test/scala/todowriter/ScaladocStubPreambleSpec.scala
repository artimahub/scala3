package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Regression tests: a declaration line that *starts with an annotation*
 *  (e.g. `@inline def f`, `@deprecated(...) def g`) must not be treated as an
 *  annotation-only preamble line. Otherwise the stub for one declaration is
 *  inserted above another declaration's line, corrupting the file.
 */
class ScaladocStubPreambleSpec extends AnyFlatSpec with Matchers:

  private def withTempFile(content: String)(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("todowriter-preamble")
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
      result = fixResult.newContent.getOrElse(content)
    }
    result

  it should "not treat a @inline def line as an annotation preamble of the declaration below it" in {
    // Mirrors Predef.scala: three undocumented @inline implicit defs, each
    // starting with an annotation on the declaration line. The stub for each
    // must go directly above its own line, not above the previous one.
    val content = """package test
                    |
                    |object PredefLike {
                    |  @inline implicit def genericArrayOps[T](xs: Array[T]): ArrayOps[T] = new ArrayOps(xs)
                    |  @inline implicit def booleanArrayOps(xs: Array[Boolean]): ArrayOps[Boolean] = new ArrayOps(xs)
                    |  @inline implicit def byteArrayOps(xs: Array[Byte]): ArrayOps[Byte] = new ArrayOps(xs)
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)

    // Each stub must be directly above its own declaration line.
    newContent should include(
      "  /** TODO FILL IN\n" +
        "   *\n" +
        "   *  @tparam T TODO FILL IN\n" +
        "   *  @param xs TODO FILL IN\n" +
        "   *  @return TODO FILL IN\n" +
        "   */\n" +
        "  @inline implicit def genericArrayOps[T](xs: Array[T]): ArrayOps[T] = new ArrayOps(xs)")
    newContent should include(
      "  /** TODO FILL IN\n" +
        "   *\n" +
        "   *  @param xs TODO FILL IN\n" +
        "   *  @return TODO FILL IN\n" +
        "   */\n" +
        "  @inline implicit def booleanArrayOps(xs: Array[Boolean]): ArrayOps[Boolean] = new ArrayOps(xs)")
    newContent should include(
      "  /** TODO FILL IN\n" +
        "   *\n" +
        "   *  @param xs TODO FILL IN\n" +
        "   *  @return TODO FILL IN\n" +
        "   */\n" +
        "  @inline implicit def byteArrayOps(xs: Array[Byte]): ArrayOps[Byte] = new ArrayOps(xs)")

    // The stubs must not be nested inside each other or inside the annotations.
    newContent should not include "xs: /**"
    newContent should not include "@in/**"
    newContent should not include "TO/**"
  }

  it should "not treat a @inline def line as an annotation preamble between two annotated operator defs" in {
    // Mirrors Predef.scala ArrowAssoc: `->` and `→` are both undocumented and
    // each preceded by its own @deprecated annotation. The `@inline def ->`
    // line must not be absorbed into `→`'s preamble.
    val content = """package test
                    |
                    |final class ArrowAssoc[A](private val self: A) extends AnyVal {
                    |  @deprecated("Use `->` extension method instead.", since = "3.10.0")
                    |  @inline def -> [B](y: B): (A, B) = (self, y)
                    |  @deprecated("Use `->` instead. If you still wish to display it as one character, consider using a font with programming ligatures such as Fira Code.", "2.13.0")
                    |  def →[B](y: B): (A, B) = ->(y)
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)

    // stub for `->` goes above its own @deprecated
    newContent should include(
      "  /** TODO FILL IN\n" +
        "   *\n" +
        "   *  @tparam B TODO FILL IN\n" +
        "   *  @param y TODO FILL IN\n" +
        "   *  @return TODO FILL IN\n" +
        "   */\n" +
        "  @deprecated(\"Use `->` extension method instead.\", since = \"3.10.0\")\n" +
        "  @inline def -> [B](y: B): (A, B) = (self, y)")
    // stub for `→` goes above its own @deprecated (which sits below `->`)
    newContent should include(
      "  /** TODO FILL IN\n" +
        "   *\n" +
        "   *  @tparam B TODO FILL IN\n" +
        "   *  @param y TODO FILL IN\n" +
        "   *  @return TODO FILL IN\n" +
        "   */\n" +
        "  @deprecated(\"Use `->` instead. If you still wish to display it as one character, consider using a font with programming ligatures such as Fira Code.\", \"2.13.0\")\n" +
        "  def →[B](y: B): (A, B) = ->(y)")

    // `@inline def ->` must not be split or absorbed into the other stub.
    newContent should not include "@para/**"
    newContent should not include "@in/**"
    newContent should not include "TO/**"
  }

  it should "still place stubs above a real multi-line annotation" in {
    // A genuine multi-line annotation (string arg continued with `+`) is still
    // recognized as an annotation preamble.
    val content = """package test
                    |
                    |class Match {
                    |  @deprecated("use the extension methods " +
                    |    "available on primitive types instead", since = "3.10.0")
                    |  @inline def byteWrapper(x: Byte): RichByte = new RichByte(x)
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)
    newContent should include(
      "  /** TODO FILL IN\n" +
        "   *\n" +
        "   *  @param x TODO FILL IN\n" +
        "   *  @return TODO FILL IN\n" +
        "   */\n" +
        "  @deprecated(\"use the extension methods \" +\n" +
        "    \"available on primitive types instead\", since = \"3.10.0\")\n" +
        "  @inline def byteWrapper(x: Byte): RichByte = new RichByte(x)")
  }

  it should "produce a file that still parses: no stub is nested inside an annotation string" in {
    val content = """package test
                    |
                    |object PredefLike {
                    |  @deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
                    |  @inline def byteWrapper(x: Byte): RichByte = new RichByte(x)
                    |  @deprecated("use the extension methods available on primitive types instead", since = "3.10.0")
                    |  @inline def shortWrapper(x: Short): RichShort = new RichShort(x)
                    |}
                    |""".stripMargin

    val newContent = fixedContent(content)
    // no `/** TODO FILL IN` may appear inside an annotation's string literal
    newContent should not include "extension methods /**"
    newContent should not include "methods ava/**"
  }
