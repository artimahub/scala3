package scala.annotation.internal
import annotation.StaticAnnotation

import language.experimental.captureChecking

/** An annotation to record a required capaility in the type of a throws
 *
 *  @param capability the capability that must be available to use the annotated throws type
 */
class requiresCapability(capability: Any) extends StaticAnnotation

