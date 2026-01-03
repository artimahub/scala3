package todowriter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class ScaladocInsertionBugSpec extends AnyFunSuite with Matchers:

  private def deleteRecursive(p: Path): Unit =
    if Files.exists(p) then
      if Files.isDirectory(p) then
        Files.list(p).iterator().asScala.foreach(deleteRecursive)
      Files.deleteIfExists(p)

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

      Main.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)

      // The scaladoc block should remain intact and not be mangled by an inserted TODO marker
      after should include ("*/\ndef newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)")
      // Ensure we did not insert a TODO inside the existing scaladoc (corruption pattern)
      after should not include ("/** TODO FILL IN */\n *  @tparam")
    finally
      deleteRecursive(dir)
    }