package test

/**
 * Test for overload reference.
 */
object TestAsJava {
  /** Converts a Scala `Buffer` to a Java `List`, see
    * [[TestConverters.asJava[A](b:scala\.collection\.mutable\.Buffer[A])* `TestConverters.asJava`]].
    */
  def testBuffer(): Unit = ???

  /** Converts a Scala `Map` to a Java `Map`, see
    * [[TestConverters.asJava[K,V](m:scala\.collection\.Map[K,V])* `TestConverters.asJava`]].
    */
  def testMap(): Unit = ???
}

object TestConverters {
  def asJava[A](b: scala.collection.mutable.Buffer[A]): java.util.List[A] = ???
  def asJava[K, V](m: scala.collection.Map[K, V]): java.util.Map[K, V] = ???
}