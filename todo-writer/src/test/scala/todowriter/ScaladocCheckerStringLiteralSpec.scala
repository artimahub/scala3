package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Regression tests for the "TODO FILL IN markers get inserted inside string
 *  literals" bug (artimahub/tasks#70).
 *
 *  The undocumented-declaration scan walks raw text line by line with no
 *  lexical awareness of string literals, so a line that looks like a
 *  declaration but is actually text inside a `"""..."""` string (e.g. sample
 *  code inside an `@implicitNotFound("""...""")` diagnostic message) is read
 *  as a real, undocumented API and gets a `/** TODO FILL IN */` stub inserted
 *  above it -- inside the string. The stub is not a comment there; it changes
 *  the string's value.
 */
class ScaladocCheckerStringLiteralSpec extends AnyFlatSpec with Matchers:

  // The three-quote sequence, so fixtures can embed triple-quoted string
  // literals (a plain `"""` Scala string cannot contain an unescaped `"""`).
  private val tq = "\"\"\""

  private def withTempFile(content: String)(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("todowriter-string-literal")
    val tempFile = tempDir.resolve("Test.scala")
    try
      Files.writeString(tempFile, content)
      test(tempFile)
    finally
      Files.deleteIfExists(tempFile)
      Files.deleteIfExists(tempDir)

  private def syntheticNames(content: String): List[String] =
    var result: List[String] = Nil
    withTempFile(content) { path =>
      result = ScaladocChecker.checkFile(path).results
        .filter(_.scaladoc.synthetic)
        .map(_.declaration.name)
    }
    result

  it should "not treat a declaration-shaped line inside a triple-quoted string as undocumented" in {
    // Minimal reproduction from the ticket: `implicit val f` is sample code
    // inside the @implicitNotFound message, not a real declaration.
    val content =
      "import scala.annotation.implicitNotFound\n\n" +
      s"@implicitNotFound(${tq}No Foo found. Define one like this:\n\n" +
      s"implicit val f: Foo = new Foo${tq})\n" +
      "trait Foo\n"
    val names = syntheticNames(content)
    names should not contain "f"
    names should contain("Foo")
  }

  it should "not treat sample code inside an @implicitNotFound message as undocumented" in {
    // Mirrors library/src/scala/concurrent/ExecutionContext.scala: the
    // annotation carries sample code showing how to define an ExecutionContext.
    val content =
      "import scala.annotation.implicitNotFound\n\n" +
      s"@implicitNotFound(${tq}Cannot find an implicit ExecutionContext. You might add\n" +
      "an (implicit ec: ExecutionContext) parameter to your method.\n" +
      "\n" +
      "If your application does not define an ExecutionContext elsewhere,\n" +
      "consider using Scala's global ExecutionContext by defining\n" +
      "the following:\n" +
      "\n" +
      s"implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global${tq})\n" +
      "trait ExecutionContext {\n" +
      "  def execute(runnable: Runnable): Unit\n" +
      "}\n"
    val names = syntheticNames(content)
    names should not contain "ec"
    names should contain("ExecutionContext")
    names should contain("execute")
  }

  it should "skip all declaration-shaped lines inside a triple-quoted string" in {
    val content =
      "object Messages {\n" +
      s"  val help = ${tq}You can:\n" +
      "def usage(): Unit = ()\n" +
      "class Helper\n" +
      "trait Marker\n" +
      s"val flag = true${tq}\n" +
      "}\n"
    val names = syntheticNames(content)
    names should contain noneOf ("usage", "Helper", "Marker", "flag")
    names should contain("help")
  }

  it should "not confuse a lone double-quote inside a triple-quoted string" in {
    val content =
      "object Messages {\n" +
      s"  val message = ${tq}He said \"hello\" then:\n" +
      "\n" +
      s"val x = 1${tq}\n" +
      "}\n"
    val names = syntheticNames(content)
    names should not contain "x"
    names should contain("message")
  }

  it should "not terminate a triple-quoted string at an escaped triple-quote" in {
    val content =
      "object Messages {\n" +
      s"  val message = ${tq}Shows \\${tq} in text\n" +
      "\n" +
      s"val y = 2${tq}\n" +
      "}\n"
    val names = syntheticNames(content)
    names should not contain "y"
  }

  it should "still detect a real declaration after the triple-quoted string closes" in {
    val content =
      "object Messages {\n" +
      s"  val help = ${tq}some text with\n" +
      s"val fake = 1${tq}\n" +
      "\n" +
      "  def real(): Int = 1\n" +
      "}\n"
    val names = syntheticNames(content)
    names should not contain "fake"
    names should contain("help")
    names should contain("real")
  }

  it should "handle escaped quotes in an ordinary string and still detect following declarations" in {
    val content =
      "class Foo {\n" +
      "  val msg = \"he said \\\"hi\\\"\"\n" +
      "  def bar(): Int = 1\n" +
      "}\n"
    val names = syntheticNames(content)
    names should contain("msg")
    names should contain("bar")
  }

  it should "not treat a triple-quote inside a line comment as opening a string" in {
    val content =
      "// a lone \"\"\" inside a comment\n" +
      "def real(): Int = 1\n"
    val names = syntheticNames(content)
    names should contain("real")
  }

  it should "not treat a triple-quote inside a block comment as opening a string" in {
    val content =
      "/* a lone \"\"\" inside a block comment\n" +
      "   more comment text */\n" +
      "def real(): Int = 1\n"
    val names = syntheticNames(content)
    names should contain("real")
  }

  it should "not insert a TODO stub inside a triple-quoted string when fixing" in {
    val content =
      "import scala.annotation.implicitNotFound\n\n" +
      s"@implicitNotFound(${tq}No Foo found. Define one like this:\n\n" +
      s"implicit val f: Foo = new Foo${tq})\n" +
      "trait Foo\n"
    withTempFile(content) { path =>
      val checkResult = ScaladocChecker.checkFile(path)
      val fixResult = Fixer.fixFile(path, checkResult.results)
      fixResult.newContent shouldBe defined
      val newContent = fixResult.newContent.get
      // The sample-code line inside the string must be untouched.
      newContent should include("implicit val f: Foo = new Foo")
      newContent should not include ("/** TODO FILL IN */\nimplicit val f")
      // Exactly one marker: the stub documenting `trait Foo`.
      val markers = "/\\*\\* TODO FILL IN \\*/".r.findAllIn(newContent).toList
      markers should have size 1
      newContent should include("/** TODO FILL IN */\ntrait Foo")
    }
  }

  it should "refuse to insert a stub at a line inside a string literal (defense-in-depth)" in {
    // If the undocumented-declaration scan ever regresses and produces a result
    // pointing at a line inside a string literal, the fixer must fail loudly
    // rather than silently insert a comment-shaped marker that corrupts the
    // string's value.
    val content =
      "object Messages {\n" +
      s"  val help = ${tq}text\n" +
      s"val fake = 1${tq}\n" +
      "}\n"
    val fakeStart = content.indexOf("val fake = 1")
    fakeStart should be >= 0
    val decl = Declaration(DeclKind.Val, "fake", Nil, Nil, None)
    val block = ScaladocBlock("", fakeStart, fakeStart, 3, Nil, Nil, hasReturn = false, hasThrows = false, synthetic = true)
    val result = CheckResult(block, decl, List(Issue.MissingDescription))
    an [IllegalStateException] should be thrownBy {
      Fixer.applyFixes(content, List(result))
    }
  }
