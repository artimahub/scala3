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

import scala.language.`2.13`

import Specializable._

/** Annotate type parameters on which code should be automatically
 *  specialized. For example:
 *  ```
 *    class MyList[@specialized T] ...
 *  ```
 *
 *  Type T can be specialized on a subset of the primitive types by
 *  specifying a list of primitive types to specialize at:
 *  ```
 *    class MyList[@specialized(Int, Double, Boolean) T] ..
 *  ```
 *
 *  @param group the group of primitive types for which specialization should be performed (typically constructed implicitly via the varargs constructor)
 */
// class tspecialized[T](group: Group[T]) extends scala.annotation.StaticAnnotation {

/** Annotates a type parameter for which the compiler should automatically generate
 *  specialized versions of the enclosing class or method, avoiding the boxing of
 *  primitive types.
 *
 *  @param group the group of primitive types for which specialization should be performed
 */
final class specialized(group: SpecializedGroup) extends scala.annotation.StaticAnnotation {
  /** Creates a specialization annotation for the given explicit list of types.
   *
   *  @param types the types at which specialization should be performed
   */
  def this(types: Specializable*) = this(new Group(types.toList))
  /** Creates a specialization annotation covering the default group of primitive types,
   *  [[scala.Specializable.Primitives]].
   */
  def this() = this(Primitives)
}
