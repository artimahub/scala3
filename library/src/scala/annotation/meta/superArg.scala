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
package meta

import scala.language.`2.13`

/** This internal annotation encodes arguments passed to annotation superclasses. Example:
 *
 * {{{
 *   class a(x: Int) extends Annotation
 *   class b extends a(42) // the compiler adds `@superArg("x", 42)` to class b
 * }}}
 *
 *  @param p TODO FILL IN
 *  @param v TODO FILL IN
 */
class superArg(p: String, v: Any) extends StaticAnnotation

/** This internal annotation encodes arguments passed to annotation superclasses. Example:
 *
 * {{{
 *   class a(x: Int) extends Annotation
 *   class b(y: Int) extends a(y) // the compiler adds `@superFwdArg("x", "y")` to class b
 * }}}
 *
 *  @param p TODO FILL IN
 *  @param n TODO FILL IN
 */
class superFwdArg(p: String, n: String) extends StaticAnnotation
