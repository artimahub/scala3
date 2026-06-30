package scala

import scala.language.`2.13`

/** An exception that indicates an error during Scala reflection.
 *
 *  @param msg the detail message describing the reflection error
 */
case class ScalaReflectionException(msg: String) extends Exception(msg)

object ScalaReflectionException extends scala.runtime.AbstractFunction1[String, ScalaReflectionException]:
  /** Returns the string `"ScalaReflectionException"`, the name of this function object. */
  override def toString(): String = "ScalaReflectionException"
