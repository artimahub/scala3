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
/** Marks a field as transient, meaning it is excluded from the default serialized
 *  form of its enclosing object. The annotated field is emitted with the JVM
 *  `transient` modifier, so its value is not written out during serialization and
 *  is restored to its default value on deserialization.
 */
final class transient extends scala.annotation.StaticAnnotation
