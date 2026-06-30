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
import scala.annotation.meta

object languageFeature {

  @meta.languageFeature("extension of type scala.Dynamic", enableRequired = true)
  /** Marker trait for the `dynamics` language feature, which permits defining direct or
   *  indirect subclasses of [[scala.Dynamic]]. Importing [[scala.language.dynamics]] brings
   *  an implicit instance into scope; without it, a direct subclass is rejected by the
   *  compiler, while an indirect subclass triggers a feature warning.
   */
  sealed trait dynamics
  object dynamics extends dynamics

  @meta.languageFeature("postfix operator #", enableRequired = true)
  /** Marker trait for the `postfixOps` language feature, which permits postfix operator
   *  notation `(expr op)`. Importing [[scala.language.postfixOps]] brings an implicit
   *  instance into scope; without it, postfix notation is rejected by the compiler.
   */
  sealed trait postfixOps
  object postfixOps extends postfixOps

  @meta.languageFeature("reflective access of structural type member #", enableRequired = false)
  /** Marker trait for the `reflectiveCalls` language feature, a legacy Scala 2 feature retained
   *  for compatibility and cross-compilation. Importing [[scala.language.reflectiveCalls]] brings
   *  an implicit instance into scope, which supplies the evidence used by the deprecated reflective
   *  conversion in [[scala.Selectable]] that enables reflective access to structural-type members;
   *  without it, that conversion is unavailable.
   */
  sealed trait reflectiveCalls
  object reflectiveCalls extends reflectiveCalls

  @meta.languageFeature("implicit conversion #", enableRequired = false)
  /** Marker trait for the `implicitConversions` language feature, which permits defining
   *  implicit conversion methods. Importing [[scala.language.implicitConversions]] brings an
   *  implicit instance into scope; without it, defining such a method triggers a compiler warning.
   */
  sealed trait implicitConversions
  object implicitConversions extends implicitConversions

  @deprecated("scala.language.higherKinds no longer needs to be imported explicitly", "2.13.1")
  @meta.languageFeature("higher-kinded type", enableRequired = false)
  /** Marker trait for the `higherKinds` language feature, a deprecated compatibility marker for
   *  Scala 2 source compatibility. In Scala 3 writing higher-kinded types needs no language import,
   *  so the import is no longer required. Importing [[scala.language.higherKinds]] brings an
   *  implicit instance into scope.
   */
  sealed trait higherKinds
  @deprecated("scala.language.higherKinds no longer needs to be imported explicitly", "2.13.1")
  object higherKinds extends higherKinds

  @meta.languageFeature("#, which cannot be expressed by wildcards,", enableRequired = false)
  /** Marker trait for the `existentials` language feature, a legacy Scala 2 feature that is no
   *  longer supported in Scala 3 and retained only for source compatibility. Importing
   *  [[scala.language.existentials]] brings an implicit instance into scope.
   */
  sealed trait existentials
  object existentials extends existentials

  object experimental {
    @meta.languageFeature("macro definition", enableRequired = true)
    /** Marker trait for the experimental `macros` language feature, which permits defining
     *  Scala 2 macros. Importing [[scala.language.experimental.macros]] brings an implicit
     *  instance into scope; without it, a Scala 2 macro definition is rejected by the compiler.
     */
    sealed trait macros
    object macros extends macros
  }
}

