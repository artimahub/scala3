package scaladoc_todo_marker

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets

class ScaladocTodoMarkerExamplesSpec extends AnyFunSuite:

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

  test("inserts TODO before top-level def (object member)") {
    val before =
      """object Top {
        |  def foo(x: Int): Int = x + 1
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "def foo(x: Int): Int = x + 1")
  }

  test("does not insert TODO for private def") {
    val before =
      """object Secrets {
        |  private def secret(): Int = 42
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isEmpty, "private defs should not produce TODO markers")
  }

  test("does not insert TODO for local def inside method") {
    val before =
      """object Local {
        |  def outer(): Int =
        |    val a = 1
        |    def inner() = a + 1
        |    inner()
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    // file may get a TODO for outer; ensure inner itself is not marked
    val (_, newLines) = result.get
    val innerIdx = newLines.indexWhere(_.contains("def inner()"))
    assert(innerIdx >= 0, "inner def not found")
    assert(newLines(innerIdx - 1).trim != "/** TODO FILL IN */", "local def 'inner' should not be annotated with TODO")
  }

  test("inserts TODO before public def inside class") {
    val before =
      """class C {
        |  def bar(): String =
        |    "ok"
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "def bar(): String =")
  }

  test("does not insert TODO when def already has Scaladoc") {
    val before =
      """object WithDoc {
        |  /** existing doc */
        |  def documented(): Int = 1
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isEmpty, "should not insert TODO when def already has Scaladoc")
  }

  test("does not insert TODO when val/var already have Scaladoc") {
    val before =
      """object WithValDoc {
        |  /** x doc */
        |  val x = 1
        |  /** y doc */
        |  var y = 2
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isEmpty, "should not insert TODO when val/var already have Scaladoc")
  }

  test("inserts TODO for class without Scaladoc") {
    val before =
      """class NoDoc {
        |  def m(): Int = 1
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "class NoDoc {")
  }

  test("inserts TODO for trait without Scaladoc") {
    val before =
      """trait NoDocT {
        |  def t(): Unit
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "trait NoDocT {")
  }

  test("inserts TODO for object without Scaladoc") {
    val before =
      """object NoDocObj {
        |  def x = 42
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isDefined)
    val (_, newLines) = result.get
    assertTodoAbove(newLines, "object NoDocObj {")
  }

  test("does not insert TODO for class with Scaladoc") {
    val before =
      """/**
        | * class doc
        | */
        |class DocC {
        |  def m(): Int = 1
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isEmpty, "should not insert TODO when class already has Scaladoc")
  }

  test("does not insert TODO for trait with Scaladoc") {
    val before =
      """/**
        | * trait doc
        | */
        |trait DocT {
        |  def t(): Unit
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isEmpty, "should not insert TODO when trait already has Scaladoc")
  }

  test("does not insert TODO for object with Scaladoc") {
    val before =
      """/**
        | * object doc
        | */
        |object DocObj {
        |  def x = 42
        |}""".stripMargin
    val tmp = writeTemp(before)
    val result = ScaladocChecker.processFile(tmp)
    assert(result.isEmpty, "should not insert TODO when object already has Scaladoc")
  }

  test("regression: elidable.scala final vals should get TODO markers") {
    // use repository file to reproduce real-case formatting
    val p = Paths.get("../../library/src/scala/annotation/elidable.scala")
    assert(Files.exists(p), s"expected file exists: $p")
    val result = ScaladocChecker.processFile(p)
    // this is a regression test that should fail before fixing the checker
    assert(result.isDefined, "expected processFile to produce edits for elidable.scala")
    val (_, newLines) = result.get

    def assertTodoFor(sub: String): Unit =
      val idx = newLines.indexWhere(_.contains(sub))
      assert(idx >= 0, s"declaration '$sub' not found in result")
      assert(idx > 0, s"no line above declaration '$sub' to check")
      assert(newLines(idx - 1).trim == "/** TODO FILL IN */", s"expected TODO above '$sub', found: '${newLines(idx-1)}'")

    assertTodoFor("final val FINEST  = 300")
    assertTodoFor("final val FINER   = 400")
    assertTodoFor("final val FINE    = 500")
  }