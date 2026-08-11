/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package util.hashing

import scala.language.`2.13`
import scala.annotation.implicitNotFound

/** `Hashing` is a trait whose instances each represent a strategy for hashing
  * instances of a type.
  *
  * `Hashing`'s companion object defines a default hashing strategy for all
  * objects - it calls their `##` method.
  *
  * Note: when using a custom `Hashing`, make sure to use it with the `Equiv`
  * such that if any two objects are equal, then their hash codes must be equal.
  */
@implicitNotFound(msg = "No implicit Hashing defined for ${T}.")
trait Hashing[T] extends Serializable {
  /** TODO FILL IN
   *
   *  @param x TODO FILL IN
   *  @return TODO FILL IN
   */
  def hash(x: T): Int
}

object Hashing {
  /** TODO FILL IN
   *
   *  @tparam T TODO FILL IN
   */
  final class Default[T] extends Hashing[T] {
    /** TODO FILL IN
     *
     *  @param x TODO FILL IN
     */
    def hash(x: T) = x.##
  }

  /** TODO FILL IN
   *
   *  @tparam T TODO FILL IN
   *  @return TODO FILL IN
   */
  implicit def default[T]: Default[T] = new Default[T]

  /** TODO FILL IN
   *
   *  @tparam T TODO FILL IN
   *  @param f TODO FILL IN
   */
  def fromFunction[T](f: T => Int) = new Hashing[T] {
    def hash(x: T) = f(x)
  }
}
