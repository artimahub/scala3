package scala.annotation

import language.experimental.captureChecking

/** A legacy annotation that was once used from Scala 2 to mark a trait as
 *  transparent. It is now deprecated and no longer has this effect: transparent
 *  traits, which are not inferred when combined with other types in an
 *  intersection, are expressed with the `transparent` modifier instead.
 *  See reference/other-new-features/transparent-traits.html for details.
 */
@deprecated(message = "Transparent traits/classes via annotations is no longer supported. Use instead the `transparent` modifier", since = "3.8.0")
/** A deprecated, legacy annotation that no longer has any effect in current
 *  Scala 3. It once marked a trait as transparent so that it would not be
 *  inferred when combined with other types in an intersection, but the compiler
 *  no longer recognizes it for this purpose. Use the `transparent` modifier
 *  instead.
 */
final class transparentTrait extends StaticAnnotation
