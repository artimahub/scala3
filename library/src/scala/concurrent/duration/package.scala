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

package scala.concurrent

import scala.language.`2.13`
import scala.language.implicitConversions

package object duration {
  /**
   * This object can be used as closing token if you prefer dot-less style but do not want
   * to enable language.postfixOps:
   *
   * ```scala sc:compile
   * import scala.concurrent.duration.*
   *
   * val duration = 2 seconds span
   * ```

   */
  object span

  /**
   * This object can be used as closing token for declaring a deadline at some future point
   * in time:
   *
   * ```scala sc:compile
   * import scala.concurrent.duration.*
   *
   * val deadline = 3 seconds fromNow
   * ```

   */
  object fromNow

  type TimeUnit          = java.util.concurrent.TimeUnit
  /** TODO FILL IN */
  final val DAYS         = java.util.concurrent.TimeUnit.DAYS
  /** TODO FILL IN */
  final val HOURS        = java.util.concurrent.TimeUnit.HOURS
  /** TODO FILL IN */
  final val MICROSECONDS = java.util.concurrent.TimeUnit.MICROSECONDS
  /** TODO FILL IN */
  final val MILLISECONDS = java.util.concurrent.TimeUnit.MILLISECONDS
  /** TODO FILL IN */
  final val MINUTES      = java.util.concurrent.TimeUnit.MINUTES
  /** TODO FILL IN */
  final val NANOSECONDS  = java.util.concurrent.TimeUnit.NANOSECONDS
  /** TODO FILL IN */
  final val SECONDS      = java.util.concurrent.TimeUnit.SECONDS

  /** TODO FILL IN
   *
   *  @param p TODO FILL IN
   *  @return TODO FILL IN
   */
  implicit def pairIntToDuration(p: (Int, TimeUnit)): Duration         = Duration(p._1.toLong, p._2)
  /** TODO FILL IN
   *
   *  @param p TODO FILL IN
   *  @return TODO FILL IN
   */
  implicit def pairLongToDuration(p: (Long, TimeUnit)): FiniteDuration = Duration(p._1, p._2)
  /** TODO FILL IN
   *
   *  @param d TODO FILL IN
   *  @return TODO FILL IN
   */
  implicit def durationToPair(d: Duration): (Long, TimeUnit)           = (d.length, d.unit)

  /** TODO FILL IN
   *
   *  @param n TODO FILL IN
   */
  implicit final class DurationInt(private val n: Int) extends AnyVal with DurationConversions {
    /** TODO FILL IN
     *
     *  @param unit TODO FILL IN
     *  @return TODO FILL IN
     */
    override protected def durationIn(unit: TimeUnit): FiniteDuration  = Duration(n.toLong, unit)
  }

  /** TODO FILL IN
   *
   *  @param n TODO FILL IN
   */
  implicit final class DurationLong(private val n: Long) extends AnyVal with DurationConversions {
    /** TODO FILL IN
     *
     *  @param unit TODO FILL IN
     *  @return TODO FILL IN
     */
    override protected def durationIn(unit: TimeUnit): FiniteDuration  = Duration(n, unit)
  }

  /** TODO FILL IN
   *
   *  @param d TODO FILL IN
   */
  implicit final class DurationDouble(private val d: Double) extends AnyVal with DurationConversions {
    /** TODO FILL IN
     *
     *  @param unit TODO FILL IN
     *  @return TODO FILL IN
     */
    override protected def durationIn(unit: TimeUnit): FiniteDuration  =
      Duration(d, unit) match {
        case f: FiniteDuration => f
        case _ => throw new IllegalArgumentException("Duration DSL not applicable to " + d)
      }
  }

  /*
   * Avoid reflection based invocation by using non-duck type
   */
  /** TODO FILL IN
   *
   *  @param i TODO FILL IN
   */
  implicit final class IntMult(private val i: Int) extends AnyVal {
    /** TODO FILL IN
     *
     *  @param d TODO FILL IN
     *  @return TODO FILL IN
     */
    def *(d: Duration): Duration             = d * i.toDouble
    /** TODO FILL IN
     *
     *  @param d TODO FILL IN
     *  @return TODO FILL IN
     */
    def *(d: FiniteDuration): FiniteDuration = d * i.toLong
  }

  /** TODO FILL IN
   *
   *  @param i TODO FILL IN
   */
  implicit final class LongMult(private val i: Long) extends AnyVal {
    /** TODO FILL IN
     *
     *  @param d TODO FILL IN
     *  @return TODO FILL IN
     */
    def *(d: Duration): Duration             = d * i.toDouble
    /** TODO FILL IN
     *
     *  @param d TODO FILL IN
     *  @return TODO FILL IN
     */
    def *(d: FiniteDuration): FiniteDuration = d * i.toLong
  }

  /** TODO FILL IN
   *
   *  @param f TODO FILL IN
   */
  implicit final class DoubleMult(private val f: Double) extends AnyVal {
    /** TODO FILL IN
     *
     *  @param d TODO FILL IN
     *  @return TODO FILL IN
     */
    def *(d: Duration): Duration             = d * f.toDouble
  }
}
