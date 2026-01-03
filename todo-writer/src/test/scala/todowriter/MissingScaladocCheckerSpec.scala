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