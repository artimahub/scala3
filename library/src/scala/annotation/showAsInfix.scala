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

package scala.annotation

import scala.language.`2.13`

/**
  * This annotation configures how Scala prints two-parameter generic types.
  *
  * By default, types with symbolic names are printed infix; while types without
  * them are printed using the regular generic type syntax.
  *
  * Example of usage:
  *  ```
  *    scala> class Map[T, U]
  *    defined class Map
  *    scala> def foo: Int Map Int = ???
  *    foo: Map[Int,Int]
  *    scala> @showAsInfix class Map[T, U]
  *    defined class Map
  *    scala> def foo: Int Map Int = ???
  *    foo: Int Map Int
  *  ```
  *
  * @param enabled when `true`, prints an otherwise non-infix two-parameter type using infix
  *                syntax; when `false`, this annotation no longer requests infix printing, though
  *                types that are infix by other rules (symbolic or `infix` names) still print infix.

  */
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
/** Configures whether a two-parameter generic type is printed using infix type syntax.
 *
 *  @param enabled when `true`, prints an otherwise non-infix two-parameter type using infix
 *                 syntax; when `false`, this annotation no longer requests infix printing, though
 *                 types that are infix by other rules (symbolic or `infix` names) still print infix
 */
class showAsInfix(enabled: Boolean = true) extends annotation.StaticAnnotation
