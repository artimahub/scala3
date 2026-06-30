package scala.annotation

import language.experimental.captureChecking

import scala.annotation.meta.*

/** https://github.com/scala/scala.github.com/pull/491 */

@field
@getter
@beanGetter
@beanSetter
@param
@setter
/** Marks a member of a companion object so that the compiler emits it as a
 *  static field or method on the companion class, like a Java `static` member.
 */
final class static extends StaticAnnotation
