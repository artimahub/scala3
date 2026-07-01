package scala

import language.experimental.captureChecking

/** A marker trait for objects that support structural selection via
 *  `selectDynamic` and `applyDynamic`
 *
 *  Implementation classes should define, or make available as extension
 *  methods, the following two method signatures:
 *  ```
 *    def selectDynamic(name: String): Any
 *    def applyDynamic(name: String)(args: Any*): Any =
 *  ```
 *  `selectDynamic` is invoked for simple selections `v.m`, whereas
 *  `applyDynamic` is invoked for selections with arguments `v.m(...)`.
 *  If there's only one kind of selection, the method supporting the
 *  other may be omitted. The `applyDynamic` can also have a second parameter
 *  list of `java.lang.Class` arguments, i.e. it may alternatively have the
 *  signature
 *  ```
 *    def applyDynamic(name: String, paramClasses: Class[?]*)(args: Any*): Any
 *  ```
 *  In this case the call will synthesize `Class` arguments for the erasure of
 *  all formal parameter types of the method in the structural type.
 */
trait Selectable extends Any

object Selectable:
  /* Scala 2 compat + allowing for cross-compilation:
   * enable scala.reflect.Selectable.reflectiveSelectable when there is an
   * import scala.language.reflectiveCalls in scope.
   */
  @deprecated(
    "import scala.reflect.Selectable.reflectiveSelectable instead of scala.language.reflectiveCalls",
    since = "3.0")
  /** Wraps a value in a [[scala.reflect.Selectable]] so that structural
   *  selections are performed reflectively on it, enabled when
   *  `scala.language.reflectiveCalls` is in scope. Provided for Scala 2
   *  compatibility and cross-compilation.
   *
   *  @param x the value to wrap for reflection-based structural member access
   *  @return `x` wrapped in a `scala.reflect.Selectable`
   */
  implicit def reflectiveSelectableFromLangReflectiveCalls(x: Any)(
      using scala.languageFeature.reflectiveCalls): scala.reflect.Selectable =
    scala.reflect.Selectable.reflectiveSelectable(x)

  /** A marker trait for subclasses of `Selectable` indicating
   *  that precise parameter types are not needed for method dispatch. That is,
   *  a class inheriting from this trait and implementing
   *  ```
   *     def applyDynamic(name: String, paramTypes: Class[?]*)(args: Any*)
   *  ```
   *  should dispatch to a method with the given `name` without having to rely
   *  on the precise `paramTypes`. Subtypes of `WithoutPreciseParameterTypes`
   *  can have more relaxed subtyping rules for refinements. They do not need
   *  the additional restriction that the signatures of the refinement and
   *  the definition that implements the refinement must match.
   */
  trait WithoutPreciseParameterTypes extends Selectable
end Selectable
