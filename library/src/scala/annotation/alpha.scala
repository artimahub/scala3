package scala.annotation

import language.experimental.captureChecking

/** A deprecated annotation that was intended to define an external name for a
 *  definition, so that an `alpha(extname)` on a method or other definition would
 *  cause its implementation to use the name `extname` instead of the regular name.
 *  Use [[targetName]] instead, which is the annotation the compiler recognizes for
 *  external-name behavior.
 */
@deprecated("use @targetName instead")
/** A deprecated annotation that was intended to define an external name for a
 *  definition, so that the annotated definition would be implemented under
 *  `externalName` instead of its regular source name. Use [[targetName]] instead,
 *  which is the annotation the compiler recognizes for external-name behavior.
 *
 *  @param externalName the name under which the annotated definition was intended to be implemented
 */
final class alpha(externalName: String) extends StaticAnnotation
