package tests

/**
 * Test case for overloaded method references.
 */
object OverloadReferenceTest {

  /** Converts a Scala `Buffer` to a Java `List`, see
    * [[AsJavaConverters.asJava[A](b:scala\.collection\.mutable\.Buffer[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
    */
  def testBufferReference(): Unit = ???

  /** Converts a Scala `Map` to a Java `Map`, see
    * [[AsJavaConverters.asJava[K,V](m:scala\.collection\.Map[K,V])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
    */
  def testMapReference(): Unit = ???

  /** Converts a Scala `Seq` to a Java `List`, see
    * [[AsJavaConverters.asJava[A](s:scala\.collection\.Seq[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
    */
  def testSeqReference(): Unit = ???
}