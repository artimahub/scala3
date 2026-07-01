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

package scala.beans

import scala.language.`2.13`
import scala.annotation.meta.{beanGetter, beanSetter, field}

/** A field annotation that generates a Java Bean-style getter named
 *  `isFieldName` for the annotated `val` or `var`, and a setter when it is a
 *  `var`.
 */
@field @beanGetter @beanSetter
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
class BooleanBeanProperty extends scala.annotation.StaticAnnotation
