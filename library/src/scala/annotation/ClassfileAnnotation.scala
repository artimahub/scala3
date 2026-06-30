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

/** A base class for classfile annotations. These are stored as
 *  [Java annotations](https://docs.oracle.com/javase/8/docs/technotes/guides/language/annotations.html)
 *  in classfiles.
 */
@deprecated("Annotation classes need to be written in Java in order to be stored in classfiles in a Java-compatible manner", "2.13.0")
/** A base trait for annotations that are stored as Java annotations in classfiles.
 *
 *  Defining such annotations in Scala is deprecated, since they cannot be persisted in a
 *  Java-compatible manner; annotation classes intended for classfile storage must be written in Java.
 */
trait ClassfileAnnotation extends ConstantAnnotation
