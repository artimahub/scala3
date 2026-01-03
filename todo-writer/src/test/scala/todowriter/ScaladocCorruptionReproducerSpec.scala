package todowriter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class ScaladocCorruptionReproducerSpec extends AnyFunSuite with Matchers:

  private def deleteRecursive(p: Path): Unit =
    if Files.exists(p) then
      if Files.isDirectory(p) then
        Files.list(p).iterator().asScala.foreach(deleteRecursive)
      Files.deleteIfExists(p)

  test("reproduce scaladoc corruption (expected to fail on fixed code)") {
    val dir = Files.createTempDirectory("todo-corrupt-repro")
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
          | *  @tparam T description
          | *  @param t description
          | */
          |def newBuilder[T](implicit t: ClassTag[T]): ArrayBuilder[T] = ArrayBuilder.make[T](using t)
          |""".stripMargin
      Files.writeString(file, content)

      // run insertion (non-dry) to trigger any corruption
      Main.insertMissingScaladocTodos(dir, false)

      val after = Files.readString(file)

      // This assertion encodes the corruption pattern we observed previously:
      // a TODO marker getting injected *inside* an existing scaladoc signature area.
      // The test expects the buggy corruption to appear (so it should FAIL with the current fixed code).
      assert(after.contains("/** TODO FILL IN */\n *  @tparam"), "expected corruption pattern not found (test should fail on fixed code)")
    finally
      deleteRecursive(dir)
    }