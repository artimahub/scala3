package scaladoc_todo_marker

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets

class ScaladocCheckerSpec extends AnyFunSuite:

  private def writeTemp(content: String): Path =
    val tmp = Files.createTempFile("scaladoc_test", ".scala")
    Files.write(tmp, content.getBytes(StandardCharsets.UTF_8))
    tmp

  private def assertTodoAbove(newLines: Seq[String], declSubstring: String): Unit =
    val idxOpt = newLines.indexWhere(_.contains(declSubstring)) match
      case -1 => None
      case i  => Some(i)
    assert(idxOpt.isDefined, s"declaration '$declSubstring' not found in result")
    val idx = idxOpt.get
    assert(idx > 0, s"no line above declaration '$declSubstring' to check")
    assert(newLines(idx - 1).trim == "/** TODO FILL IN */", s"expected TODO above '$declSubstring', found: '${newLines(idx-1)}'")

  test("inserts TODO before constant vals (elidable example)") {
    val before =
      """object elidable {
        |  final val ALL     = Int.MinValue  // Level.ALL.intValue()
        |  final val FINEST  = 300           // Level.FINEST.intValue()
        |  final val FINER   = 400           // Level.FINER.intValue()
        |  final val FINE    = 500           // Level.FINE.intValue()
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "final val FINEST  = 300")
    assertTodoAbove(newLines, "final val FINER   = 400")
    assertTodoAbove(newLines, "final val FINE    = 500")
  }

  test("inserts TODO before secondary constructor (experimental example)") {
    val before =
      """/**
        | *  @syntax markdown
        | */
        |final class experimental(message: String) extends StaticAnnotation:
        |  def this() = this("")""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "def this() = this(\"\")")
  }

  test("inserts TODO before secondary constructor in private class (preview example)") {
    val before =
      """/**
        | *  @syntax markdown
        | */
        |private[scala] final class preview(message: String) extends StaticAnnotation:
        |  def this() = this("")""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "def this() = this(\"\")")
  }

  test("inserts TODO before constructor in class with braces (unused example)") {
    val before =
      """/**
        | *  @syntax markdown
        | */
        |@meta.getter @meta.setter
        |class unused(message: String) extends StaticAnnotation {
        |  def this() = this("")
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "def this() = this(\"\")")
  }