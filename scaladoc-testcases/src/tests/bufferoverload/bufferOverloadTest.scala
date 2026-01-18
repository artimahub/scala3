package tests.bufferoverload

/**
 * Test for overload reference in BufferHasAsJava.
 * This should link to the Buffer overload of asJava, not the Map overload.
 */
object BufferOverloadTest {
  /** Converts a Scala `Buffer` to a Java `List`, see
    * [[AsJavaConverters.asJava[A](b:scala\.collection\.mutable\.Buffer[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
    */
  def testBuffer(): Unit = ???

  /** Converts a Scala `Map` to a Java `Map`, see
    * [[AsJavaConverters.asJava[K,V](m:scala\.collection\.Map[K,V])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
    */
  def testMap(): Unit = ???
}