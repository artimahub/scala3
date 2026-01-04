package todowriter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class MissingScaladocCheckerSpec extends AnyFunSuite with Matchers:

  private def deleteRecursive(p: Path): Unit =
    if Files.exists(p) then
      if Files.isDirectory(p) then
        Files.list(p).iterator().asScala.foreach(deleteRecursive)
      Files.deleteIfExists(p)

  test("dry run does not modify files") {
    val dir = Files.createTempDirectory("todo-test-dry")
    try
      val file = dir.resolve("A.scala")
      val content = "package a\n\nclass A\n"
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, true)

      val after = Files.readString(file)
      after shouldEqual content
    finally
      deleteRecursive(dir)
    }
  
  test("inserts TODO before public/protected declarations") {
    val dir = Files.createTempDirectory("todo-test-insert")
    try
      val file = dir.resolve("B.scala")
      val content =
        """package b
          |
          |class B
          |
          |trait C
          |
          |object D
          |
          |def foo(x: Int): Int = x
          |
          |private class Hidden
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)

      // Should have TODO before public declarations
      assert(after.contains("/** TODO FILL IN */\nclass B"), "missing TODO for class B")
      assert(after.contains("/** TODO FILL IN */\ntrait C"), "missing TODO for trait C")
      assert(after.contains("/** TODO FILL IN */\nobject D"), "missing TODO for object D")
      assert(after.contains("/** TODO FILL IN */\ndef foo"), "missing TODO for def foo")

      // Should NOT insert before private declaration
      assert(!after.contains("/** TODO FILL IN */\nprivate class Hidden"), "should not insert before private class")
    finally
      deleteRecursive(dir)
    }
  
  test("does not insert when scaladoc already exists") {
    val dir = Files.createTempDirectory("todo-test-noscaladoc")
    try
      val file = dir.resolve("C.scala")
      val content =
        """package c
          |
          |/** Existing doc */
          |class WithDoc
          |
          |/** Another doc */
          |def bar(x: Int): Int = x
          |
          |trait NoDoc
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)

      // Should NOT insert before declarations that already have scaladoc
      assert(!after.contains("/** TODO FILL IN */\nclass WithDoc"), "should not insert before class that has scaladoc")
      assert(!after.contains("/** TODO FILL IN */\ndef bar"), "should not insert before def that has scaladoc")

      // Should insert for the trait without scaladoc
      assert(after.contains("/** TODO FILL IN */\ntrait NoDoc"), "missing TODO for trait NoDoc")
    finally
      deleteRecursive(dir)
    }

  test("does not corrupt existing scaladoc when inserting todos") {
    val dir = Files.createTempDirectory("todo-scaladoc-bug")
    try
      val file = dir.resolve("ArrayFactory.scala")
      val content =
        """package scala.collection.mutable
          |
          |private class ArrayFactory[A : ClassTag](dummy: Array.type) extends Factory[A, Array[A]] with Serializable {
          |  def fromSpecific(it: IterableOnce[A]): Array[A] = Array.from[A](it)
          |  def newBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder[A]
          |}
          |
          |/**
          | * Returns a new [[scala.collection.mutable.ArrayBuilder]].
          | *
          | *  @tparam T TODO FILL IN
          | *  @param t TODO FILL IN
          | */
          |def newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)

      // The scaladoc block should remain intact and not be mangled by an inserted TODO marker
      after should include ("*/\ndef newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)")
      // Ensure we did not insert a TODO inside the existing scaladoc (corruption pattern)
      after should not include ("/** TODO FILL IN */\n *  @tparam")
    finally
      deleteRecursive(dir)
    }
  
  test("does not insert TODO when scaladoc immediately follows code without blank line") {
    val dir = Files.createTempDirectory("todo-scaladoc-immediate")
    try
      val file = dir.resolve("Immediate.scala")
      val content =
        """package p
          |
          |object X{
          |  def helper() = ()
          |}/**
          | * Docs for newBuilder
          | * @tparam T TODO FILL IN
          | * @param t TODO FILL IN
          | */
          |def newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)
      // The comment start should remain directly before def, not preceded by a TODO
      assert(!after.contains("/** TODO FILL IN */\n/**"), "should not insert TODO before existing scaladoc start")
      // Ensure comment body preserved
      after should include ("* @tparam T TODO FILL IN")
    finally
      deleteRecursive(dir)
    }

  test("does not insert inside indented scaladoc with leading asterisks") {
    val dir = Files.createTempDirectory("todo-scaladoc-indented")
    try
      val file = dir.resolve("Indented.scala")
      val content =
        """package q
          |
          |class Y
          |
          |  /**
          |   * Multi-line comment with leading spaces
          |   *  @tparam A TODO FILL IN
          |   *  @param a TODO FILL IN
          |   */
          |  def newBuilder[A](implicit a: ClassTag[A]): ArrayBuilder[A] = ArrayBuilder.make[A](using a)
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)
      // Should not insert a TODO before the indented scaladoc
      assert(!after.contains("/** TODO FILL IN */\n  /**"), "should not insert TODO before indented scaladoc")
      // Ensure the indented @tparam line remains intact
      after should include ("*  @tparam A TODO FILL IN")
    finally
      deleteRecursive(dir)
    }

  // New regression test reproducing reported corruption: a method with the same name exists inside a nearby class
  // and a top-level scaladoc follows; ensure we don't mangle the scaladoc or remove the inner method.
  test("regression: does not mangle scaladoc when class method and top-level scaladoc share a name") {
    val dir = Files.createTempDirectory("todo-scaladoc-regression")
    try
      val file = dir.resolve("Regress.scala")
      val content =
        """package regress
          |
          |object Array {
          |  private class ArrayFactory[A : ClassTag](dummy: Array.type) extends Serializable {
          |    def fromSpecific(it: IterableOnce[A]): Array[A] = Array.from[A](it)
          |    def newBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder[A]
          |  }
          |
          |  /**
          |   * Returns a new [[scala.collection.mutable.ArrayBuilder]].
          |   *
          |   *  @tparam T TODO FILL IN
          |   *  @param t TODO FILL IN
          |   */
          |  def newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)
          |}
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)

      // inner method should remain
      after should include ("def newBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder[A]")
      // scaladoc should remain intact and not have an inserted TODO inside it
      after should include ("*  @tparam T TODO FILL IN")
      after should not include ("/** TODO FILL IN */\n *  @tparam")
    finally
      deleteRecursive(dir)
    }

  // ---- tests moved from MissingScaladocCheckerArrayRealSpec.scala ----

  test("real Array.scala snippet preserves scaladoc and inner methods when inserting TODOs") {
    val dir = Files.createTempDirectory("msctest-array-real")
    try
      val file = dir.resolve("Array.scala")
      val content =
        """package scala
          |
          |import scala.collection.{Factory, immutable, mutable}
          |import mutable.ArrayBuilder
          |import immutable.ArraySeq
          |import scala.reflect.{ClassTag, classTag}
          |
          |implicit def toFactory[A : ClassTag](dummy: Array.type): Factory[A, Array[A]] = new ArrayFactory(dummy)
          |@SerialVersionUID(3L)
          |private class ArrayFactory[A : ClassTag](dummy: Array.type) extends Factory[A, Array[A]] with Serializable {
          |  def fromSpecific(it: IterableOnce[A]): Array[A] = Array.from[A](it)
          |  def newBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder[A]
          |}
          |
          |/**
          | * Returns a new [[scala.collection.mutable.ArrayBuilder]].
          | */
          |def newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)

      // The scaladoc block should remain intact and not be mangled by an inserted TODO marker
      after should include ("*/\ndef newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)")
      // inner class method should remain
      after should include ("def newBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder[A]")
      // Ensure no TODO was inserted inside the existing scaladoc block
      after should not include ("/** TODO FILL IN */\n *")
    finally
      deleteRecursive(dir)
    }

  // ---- tests moved from MissingScaladocCheckerArrayScalaSpec.scala ----

  test("running checker on Array.scala snippet does not corrupt existing scaladoc") {
    val dir = Files.createTempDirectory("msctest-array-scala")
    try
      val file = dir.resolve("Array.scala")
      val content =
        """package scala
          |
          |import scala.collection.{Factory, immutable, mutable}
          |import mutable.ArrayBuilder
          |import immutable.ArraySeq
          |import scala.reflect.{ClassTag, classTag}
          |
          |implicit def toFactory[A : ClassTag](dummy: Array.type): Factory[A, Array[A]] = new ArrayFactory(dummy)
          |@SerialVersionUID(3L)
          |private class ArrayFactory[A : ClassTag](dummy: Array.type) extends Factory[A, Array[A]] with Serializable {
          |  def fromSpecific(it: IterableOnce[A]): Array[A] = Array.from[A](it)
          |  def newBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder[A]
          |}
          |
          |/**
          | * Returns a new [[scala.collection.mutable.ArrayBuilder]].
          | */
          |def newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)

      // scaladoc block for the top-level newBuilder must remain intact
      after should include ("*/\ndef newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)")
      // inner class method should remain unchanged
      after should include ("def newBuilder: mutable.Builder[A, Array[A]] = Array.newBuilder[A]")
      // ensure we did not insert a TODO inside the existing scaladoc block
      after should not include ("/** TODO FILL IN */\n *")
    finally
      deleteRecursive(dir)
    }

  // ---- tests moved from MissingScaladocCheckerExtraSpec.scala ----

  test("attached scaladoc immediately before declaration is preserved (no insertion)") {
    val dir = Files.createTempDirectory("msctest-attached")
    try
      val file = dir.resolve("A.scala")
      val content =
        """object A {
          |  /**
          |   * Existing doc.
          |   */
          |  def foo(): Int = 1
          |}""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)
  
      val after = Files.readString(file)
      after should include("Existing doc.")
      // object A has no scaladoc, so a TODO may be inserted before it; ensure inner doc is preserved
      after should include("/** TODO FILL IN */\nobject A")
      after should not include("/** TODO FILL IN */\n  /**")
    finally
      deleteRecursive(dir)
  }

  test("annotation/modifier lines between scaladoc and decl are allowed (no insertion)") {
    val dir = Files.createTempDirectory("msctest-annotation")
    try
      val file = dir.resolve("B.scala")
      val content =
        """/**
          | * Doc for foo
          | */
          |@deprecated
          |protected def foo(): Unit = ()
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)
      after should not include("TODO FILL IN")
      after should include("@deprecated")
    finally
      deleteRecursive(dir)
    }

  test("non-modifier lines between scaladoc and decl mean scaladoc is NOT attached (insertion occurs)") {
    val dir = Files.createTempDirectory("msctest-nonmod")
    try
      val file = dir.resolve("C.scala")
      val content =
        """/** Doc for previous item */
          |// some comment that is not an annotation or modifier
          |def missing(): Unit = ()
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)
      // TODO should be inserted before the def because intervening line is not modifier-only/annotation/blank
      after should include("/** TODO FILL IN */")
    finally
      deleteRecursive(dir)
    }

  test("declaration lines that are inside an existing scaladoc block are not modified") {
    val dir = Files.createTempDirectory("msctest-inside")
    try
      val file = dir.resolve("D.scala")
      val content =
        """/**
          | * Malformed scaladoc starts but does not end
          | * def insideComment(): Int = 2
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)
      // Nothing should be injected inside the comment; file should remain unchanged structure-wise
      after should include("Malformed scaladoc")
      after should not include("TODO FILL IN")
    finally
      deleteRecursive(dir)
    }

  test("private declarations are skipped (no insertion)") {
    val dir = Files.createTempDirectory("msctest-private")
    try
      val file = dir.resolve("E.scala")
      val content =
        """private def hidden(): Unit = ()
          |protected[this] def alsoHidden(): Int = 0
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)
      after should not include("TODO FILL IN")
    finally
      deleteRecursive(dir)
    }

  test("scaladoc belonging to a different nearby declaration is not reused (insertion occurs for unrelated decl)") {
    val dir = Files.createTempDirectory("msctest-different")
    try
      val file = dir.resolve("F.scala")
      val content =
        """/**
          | * Doc for alpha
          | */
          |def alpha(): Unit = ()
          |
          |// some spacer comment
          |def beta(): Unit = ()
          |""".stripMargin
      Files.writeString(file, content)

      MissingScaladocChecker.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)
      // alpha has doc, beta should receive a TODO because there is an intervening non-modifier comment line
      after should include("/** TODO FILL IN */")
      // Ensure we did not mutate alpha's scaladoc
      after should include("Doc for alpha")
    finally
      deleteRecursive(dir)
    }