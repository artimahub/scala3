package scala.annotation
package internal

import language.experimental.captureChecking

/** Marks a capture reference as a reach capability, encoding `x*` as `x.type @reachCapability`. */
@deprecated("Reach capabilities are no longer supported", since = "3.10")
class reachCapability extends StaticAnnotation

