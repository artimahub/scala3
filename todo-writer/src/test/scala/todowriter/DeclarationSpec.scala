package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeclarationSpec extends AnyFlatSpec with Matchers:

  "Declaration" should "parse simple def" in {
    val chunk = "def foo(x: Int): String = ???"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.name should be("foo")
    decl.params should be(List("x"))
    decl.returnType should be(Some("String"))
  }

  it should "parse def with type params" in {
    val chunk = "def foo[A, B](x: A): B = ???"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.name should be("foo")
    decl.tparams should be(List("A", "B"))
    decl.params should be(List("x"))
    decl.returnType should be(Some("B"))
  }

  it should "parse class with params" in {
    val chunk = "class Foo[T](val x: Int, y: String)"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Class)
    decl.name should be("Foo")
    decl.tparams should be(List("T"))
    decl.params should be(List("x", "y"))
  }

  it should "parse def with multiple param lists" in {
    val chunk = "def foo(x: Int)(y: String): Unit = ???"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.params should contain allOf ("x", "y")
  }

  it should "parse def params when a param type contains nested parentheses" in {
    val chunk = "inline def map[F[_]](f: [t] => t => F[t]): Map[NamedTuple[N, V], F] = ???"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.name should be("map")
    decl.params should be(List("f"))
  }

  it should "handle implicit params" in {
    val chunk = "def foo(x: Int)(implicit ctx: Context): Unit = ???"
    val decl = Declaration.parse(chunk)
    decl.params should contain allOf ("x", "ctx")
  }

  it should "parse class params across multiple parameter lists including implicit val" in {
    val chunk = "class BufferedSource(inputStream: InputStream, bufferSize: Int)(implicit val codec: Codec) extends Source"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Class)
    decl.name should be("BufferedSource")
    decl.params should be(List("inputStream", "bufferSize", "codec"))
  }

  it should "handle using params" in {
    val chunk = "def foo(x: Int)(using ctx: Context): Unit = ???"
    val decl = Declaration.parse(chunk)
    decl.params should contain allOf ("x", "ctx")
  }

  it should "skip unnamed using params" in {
    val chunk = "def foo(x: Int)(using Context): Unit = ???"
    val decl = Declaration.parse(chunk)
    decl.params should be(List("x"))
  }

  it should "parse trait" in {
    val chunk = "trait Foo[A, B]"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Trait)
    decl.name should be("Foo")
    decl.tparams should be(List("A", "B"))
  }

  it should "parse object" in {
    val chunk = "object Foo"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Object)
    decl.name should be("Foo")
  }

  it should "handle annotations" in {
    val chunk = "@deprecated def foo(x: Int): String = ???"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.name should be("foo")
  }

  it should "handle modifiers" in {
    val chunk = "private final def foo(x: Int): String = ???"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.name should be("foo")
  }

  it should "parse val" in {
    val chunk = "val foo: Int = 42"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Val)
    decl.name should be("foo")
  }

  it should "parse var" in {
    val chunk = "var foo: Int = 42"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Var)
    decl.name should be("foo")
  }

  it should "handle type params with bounds" in {
    val chunk = "def foo[A <: Foo, B >: Bar](x: A): B = ???"
    val decl = Declaration.parse(chunk)
    decl.tparams should be(List("A", "B"))
  }

  it should "handle type params with variance" in {
    val chunk = "class Foo[+A, -B](x: A)"
    val decl = Declaration.parse(chunk)
    decl.tparams should be(List("A", "B"))
  }

  it should "detect Unit return type" in {
    val chunk = "def foo(x: Int): Unit = ???"
    val decl = Declaration.parse(chunk)
    decl.returnType should be(Some("Unit"))
  }

  it should "handle multi-line declarations" in {
    val chunk = """def foo(
                  |  x: Int,
                  |  y: String
                  |): Boolean = ???""".stripMargin
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.name should be("foo")
    decl.params should contain allOf ("x", "y")
    decl.returnType should be(Some("Boolean"))
  }

  it should "handle sealed trait" in {
    val chunk = "sealed trait Foo[A]"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Trait)
    decl.name should be("Foo")
  }

  it should "handle case class" in {
    val chunk = "case class Foo(x: Int, y: String)"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Class)
    decl.name should be("Foo")
    decl.params should be(List("x", "y"))
  }

  it should "handle abstract class" in {
    val chunk = "abstract class Foo[A](x: A)"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Class)
    decl.name should be("Foo")
    decl.tparams should be(List("A"))
    decl.params should be(List("x"))
  }

  it should "handle parameters starting with underscore" in {
    val chunk = "def foo(_i0: Int, _iN: String, x: Int): Unit = ???"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.params should contain allOf ("_i0", "_iN", "x")
  }

  it should "handle class with underscore-prefixed constructor parameters" in {
    val chunk = "class Foo(_i0: Int, _iN: String)"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Class)
    decl.name should be("Foo")
    decl.params should contain allOf ("_i0", "_iN")
  }

  it should "parse annotated parameters" in {
    val chunk = "@inline def locally[T](@DeprecatedName(\"x\") x: T): T = x"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Def)
    decl.name should be("locally")
    decl.params should be(List("x"))
  }

  it should "parse qualified private val constructor parameter names" in {
    val chunk = "class Foo(private[immutable] val len1: Int)"
    val decl = Declaration.parse(chunk)
    decl.kind should be(DeclKind.Class)
    decl.name should be("Foo")
    decl.params should be(List("len1"))
  }
