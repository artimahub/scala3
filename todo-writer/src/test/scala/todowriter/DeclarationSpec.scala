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

  it should "handle implicit params" in {
    val chunk = "def foo(x: Int)(implicit ctx: Context): Unit = ???"
    val decl = Declaration.parse(chunk)
    decl.params should contain allOf ("x", "ctx")
  }

  it should "handle using params" in {
    val chunk = "def foo(x: Int)(using ctx: Context): Unit = ???"
    val decl = Declaration.parse(chunk)
    decl.params should contain allOf ("x", "ctx")
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
