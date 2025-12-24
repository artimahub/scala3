package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Tests for optional TODO insertion in Fixer.fixFile */
class InsertTodoSpec extends AnyFlatSpec with Matchers:

  private def withTempFile(content: String)(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("todowriter-test")
    val tempFile = tempDir.resolve("Test.scala")
    try
      Files.writeString(tempFile, content)
      test(tempFile)
    finally
      Files.deleteIfExists(tempFile)
      Files.deleteIfExists(tempDir)

  "Fixer.fixFile" should "insert TODO tags when insertTodo = true" in {
    val content = """package test
                    |
                    |/** Provides an implicit conversion from the Array object to a collection Factory. */
                    |def foo[A](x: Int): Int = 1
                    |""".stripMargin

    withTempFile(content) { path =>
      val check = ScaladocChecker.checkFile(path)
      val fix = Fixer.fixFile(path, check.results, insertTodo = true)
      fix.newContent shouldBe defined
      val newContent = fix.newContent.get
      newContent should include("@param x TODO FILL IN")
    }
  }

  it should "not insert TODO tags when insertTodo = false (only adjust asterisks/alignment)" in {
    val content = """package test
                    |
                    |/** Provides an implicit conversion from the Array object to a collection Factory. */
                    |def foo[A](x: Int): Int = 1
                    |""".stripMargin

    withTempFile(content) { path =>
      val check = ScaladocChecker.checkFile(path)
      val fix = Fixer.fixFile(path, check.results, insertTodo = false)
      fix.newContent shouldBe defined
      val newContent = fix.newContent.get
      // Should not add @param/@tparam/@return
      newContent should not include ("@param")
      newContent should not include ("@tparam")
      newContent should not include ("@return")
      // Still should preserve/adjust scaladoc formatting (opening line present)
      newContent should include("/** Provides an implicit conversion from the Array object to a collection Factory.")
    }
  }