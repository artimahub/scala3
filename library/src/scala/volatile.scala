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
import scala.annotation.meta._

@field
/** Marks the annotated field as volatile, so that reads and writes of the
 *  field are not cached or reordered and are always visible across threads.
 */
final class volatile extends scala.annotation.StaticAnnotation
