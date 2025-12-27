package scala.annotation

import language.experimental.captureChecking

/** An annotation that can be used to mark a definition as experimental.
 *
 *  @see [[https://dotty.epfl.ch/docs/reference/other-new-features/experimental-defs]]
 *  @syntax markdown
 *
 *  @param message an optional message explaining why the definition is experimental
 */
final class experimental(message: String) extends StaticAnnotation:
  def this() = this("")
