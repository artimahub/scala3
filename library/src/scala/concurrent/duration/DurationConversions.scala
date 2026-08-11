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

package scala.concurrent.duration

import scala.language.`2.13`
import DurationConversions._

// Would be nice to limit the visibility of this trait a little bit,
// but it crashes scalac to do so.
/** TODO FILL IN */
trait DurationConversions extends Any {
  /** TODO FILL IN
   *
   *  @param unit TODO FILL IN
   *  @return TODO FILL IN
   */
  protected def durationIn(unit: TimeUnit): FiniteDuration

  /** TODO FILL IN */
  def nanoseconds: FiniteDuration  = durationIn(NANOSECONDS)
  /** TODO FILL IN */
  def nanos: FiniteDuration        = nanoseconds
  /** TODO FILL IN */
  def nanosecond: FiniteDuration   = nanoseconds
  /** TODO FILL IN */
  def nano: FiniteDuration         = nanoseconds

  /** TODO FILL IN */
  def microseconds: FiniteDuration = durationIn(MICROSECONDS)
  /** TODO FILL IN */
  def micros: FiniteDuration       = microseconds
  /** TODO FILL IN */
  def microsecond: FiniteDuration  = microseconds
  /** TODO FILL IN */
  def micro: FiniteDuration        = microseconds

  /** TODO FILL IN */
  def milliseconds: FiniteDuration = durationIn(MILLISECONDS)
  /** TODO FILL IN */
  def millis: FiniteDuration       = milliseconds
  /** TODO FILL IN */
  def millisecond: FiniteDuration  = milliseconds
  /** TODO FILL IN */
  def milli: FiniteDuration        = milliseconds

  /** TODO FILL IN */
  def seconds: FiniteDuration      = durationIn(SECONDS)
  /** TODO FILL IN */
  def second: FiniteDuration       = seconds

  /** TODO FILL IN */
  def minutes: FiniteDuration      = durationIn(MINUTES)
  /** TODO FILL IN */
  def minute: FiniteDuration       = minutes

  /** TODO FILL IN */
  def hours: FiniteDuration        = durationIn(HOURS)
  /** TODO FILL IN */
  def hour: FiniteDuration         = hours

  /** TODO FILL IN */
  def days: FiniteDuration         = durationIn(DAYS)
  /** TODO FILL IN */
  def day: FiniteDuration          = days

  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def nanoseconds[C](c: C)(implicit ev: Classifier[C]): ev.R  = ev.convert(nanoseconds)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def nanos[C](c: C)(implicit ev: Classifier[C]): ev.R        = nanoseconds(c)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def nanosecond[C](c: C)(implicit ev: Classifier[C]): ev.R   = nanoseconds(c)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def nano[C](c: C)(implicit ev: Classifier[C]): ev.R         = nanoseconds(c)

  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def microseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(microseconds)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def micros[C](c: C)(implicit ev: Classifier[C]): ev.R       = microseconds(c)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def microsecond[C](c: C)(implicit ev: Classifier[C]): ev.R  = microseconds(c)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def micro[C](c: C)(implicit ev: Classifier[C]): ev.R        = microseconds(c)

  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def milliseconds[C](c: C)(implicit ev: Classifier[C]): ev.R = ev.convert(milliseconds)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def millis[C](c: C)(implicit ev: Classifier[C]): ev.R       = milliseconds(c)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def millisecond[C](c: C)(implicit ev: Classifier[C]): ev.R  = milliseconds(c)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def milli[C](c: C)(implicit ev: Classifier[C]): ev.R        = milliseconds(c)

  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def seconds[C](c: C)(implicit ev: Classifier[C]): ev.R      = ev.convert(seconds)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def second[C](c: C)(implicit ev: Classifier[C]): ev.R       = seconds(c)

  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def minutes[C](c: C)(implicit ev: Classifier[C]): ev.R      = ev.convert(minutes)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def minute[C](c: C)(implicit ev: Classifier[C]): ev.R       = minutes(c)

  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def hours[C](c: C)(implicit ev: Classifier[C]): ev.R        = ev.convert(hours)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def hour[C](c: C)(implicit ev: Classifier[C]): ev.R         = hours(c)

  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def days[C](c: C)(implicit ev: Classifier[C]): ev.R         = ev.convert(days)
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   *  @param c TODO FILL IN
   *  @param ev TODO FILL IN
   *  @return TODO FILL IN
   */
  def day[C](c: C)(implicit ev: Classifier[C]): ev.R          = days(c)
}

/**
 * This object just holds some cogs which make the DSL machine work, not for direct consumption.
 */
object DurationConversions {
  /** TODO FILL IN
   *
   *  @tparam C TODO FILL IN
   */
  trait Classifier[C] {
    type R
    /** TODO FILL IN
     *
     *  @param d TODO FILL IN
     *  @return TODO FILL IN
     */
    def convert(d: FiniteDuration): R
  }

  implicit object spanConvert extends Classifier[span.type] {
    type R = FiniteDuration
    /** TODO FILL IN
     *
     *  @param d TODO FILL IN
     *  @return TODO FILL IN
     */
    def convert(d: FiniteDuration): FiniteDuration = d
  }

  implicit object fromNowConvert extends Classifier[fromNow.type] {
    type R = Deadline
    /** TODO FILL IN
     *
     *  @param d TODO FILL IN
     *  @return TODO FILL IN
     */
    def convert(d: FiniteDuration): Deadline = Deadline.now + d
  }

}
