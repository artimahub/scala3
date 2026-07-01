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

final class specialized(group: SpecializedGroup) extends scala.annotation.StaticAnnotation {
  /** Creates a `specialized` annotation that specializes on the given types.
   *
   *  @param types the types to specialize on, given as their `Specializable` companions (for example `Int`, `Double`, `Boolean`)
   */
  def this(types: Specializable*) = this(new Group(types.toList))
  /** Creates a `specialized` annotation that specializes on all primitive types. */
  def this() = this(Primitives)
}
