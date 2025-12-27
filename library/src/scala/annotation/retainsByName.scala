package scala.annotation

import language.experimental.captureChecking

/** An annotation that indicates capture of an enclosing by-name type
 *
 *  @tparam Elems a union of singleton types representing the captured capabilities
 */
@experimental class retainsByName[Elems] extends annotation.StaticAnnotation

