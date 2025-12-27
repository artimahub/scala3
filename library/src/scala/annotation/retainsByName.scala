package scala.annotation

import language.experimental.captureChecking

/** An annotation that indicates capture of an enclosing by-name type
 *
 *  @tparam Elems TODO FILL IN
 */
@experimental class retainsByName[Elems] extends annotation.StaticAnnotation

