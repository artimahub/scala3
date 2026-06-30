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

package scala.annotation.unchecked

import scala.language.`2.13`
import scala.annotation.meta.{field, getter}

/** An annotation that marks a value as stable, suppressing the compiler's check
 *  that would otherwise reject it as a prefix of a path-dependent type because
 *  its type is volatile.
 */
@getter @field
final class uncheckedStable extends scala.annotation.StaticAnnotation {}
