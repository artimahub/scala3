package tests.externalJavadoc

import java.util.*

class Test {
  def a: Map.Entry[String, String] = ???

  def b: java.util.Map[String, Int] = ???

  def c: java.util.stream.Stream.Builder[String] = ???
}

class MyException extends java.lang.Exception

class MyArrayList[T] extends java.util.ArrayList[T]

trait MyPrintStream extends java.io.PrintStream

/** Test method with javadoc link */
class MethodTest {
  /** See [[java.lang.Float.compare]] */
  def compareFloats(x: Float, y: Float): Int = java.lang.Float.compare(x, y)

  /** See [[java.lang.Double.compare]] */
  def compareDoubles(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
}

