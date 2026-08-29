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

package scala
package reflect

import scala.language.`2.13`
import scala.collection.mutable.{ArrayBuilder, ArraySeq}
import java.lang.{Class => jClass}

import scala.annotation.{nowarn, tailrec}

@deprecated("use scala.reflect.ClassTag instead", "2.10.0")
/** The deprecated members of [[scala.reflect.ClassManifest]], separated into
 *  their own trait so that each can carry its own deprecation annotation.
 *
 *  @tparam T the type represented by the manifest
 */
trait ClassManifestDeprecatedApis[T] extends OptManifest[T] {
  self: ClassManifest[T] =>

  // Still in use in target test.junit.comp.
  @deprecated("use runtimeClass instead", "2.10.0")
  /** Returns the runtime `Class` object representing the erasure of the type `T`. */
  def erasure: jClass[?] = runtimeClass

  private def subtype(sub: jClass[?], sup: jClass[?]): Boolean = {
    @tailrec
    def loop(left: Set[jClass[?]], seen: Set[jClass[?]]): Boolean = {
      left.nonEmpty && {
        val next = left.head
        val supers = next.getInterfaces.toSet ++ Option(next.getSuperclass)
        supers(sup) || {
          val xs = left ++ supers filterNot seen
          loop(xs - next, seen + next)
        }
      }
    }
    loop(Set(sub), Set())
  }

  private def subargs(args1: List[OptManifest[?]], args2: List[OptManifest[?]]) = (args1 corresponds args2) {
    // !!! [Martin] this is wrong, need to take variance into account
    case (x: ClassManifest[?], y: ClassManifest[?]) => x <:< y
    case (x, y)                                     => (x eq NoManifest) && (y eq NoManifest)
  }

  /** Tests whether the type represented by this manifest is a subtype
   *  of the type represented by `that` manifest, subject to the limitations
   *  described in the header.
   */
  @deprecated("use scala.reflect.runtime.universe.TypeTag for subtype checking instead", "2.10.0")
  def <:<(that: ClassManifest[?]): Boolean = {
    // All types which could conform to these types will override <:<.
    def cannotMatch = {
      import Manifest._
      that.isInstanceOf[AnyValManifest[?]] || (that eq AnyVal) || (that eq Nothing) || (that eq Null)
    }

    // This is wrong, and I don't know how it can be made right
    // without more development of Manifests, due to arity-defying
    // relationships like:
    //
    //   List[String] <: AnyRef
    //   Map[Int, Int] <: Iterable[(Int, Int)]
    //
    // Given the manifest for Map[K, V] how do I determine that a
    // supertype has single type argument (K, V) ? I don't see how we
    // can say whether X <:< Y when type arguments are involved except
    // when the erasure is the same, even before considering variance.
    !cannotMatch && {
      // this part is wrong for not considering variance
      if (this.runtimeClass == that.runtimeClass)
        subargs(this.typeArguments, that.typeArguments)
      // this part is wrong for punting unless the rhs has no type
      // arguments, but it's better than a blindfolded pinata swing.
      else
        that.typeArguments.isEmpty && subtype(this.runtimeClass, that.runtimeClass)
    }
  }

  /** Tests whether the type represented by this manifest is a supertype
   *  of the type represented by `that` manifest, subject to the limitations
   *  described in the header.
   */
  @deprecated("use scala.reflect.runtime.universe.TypeTag for subtype checking instead", "2.10.0")
  def >:>(that: ClassManifest[?]): Boolean =
    that <:< this

  /** Tests whether `other` may be compared for equality with this manifest,
   *  which holds exactly when `other` is itself a `ClassManifest`.
   *
   *  @param other the value tested for comparability with this manifest
   */
  override def canEqual(other: Any) = other match {
    case _: ClassManifest[?] => true
    case _                   => false
  }

  /** Returns the runtime `Class` object for the array type whose element type is `tp`.
   *
   *  @tparam A the element type of the array
   *  @param tp the runtime `Class` object of the array's element type
   */
  protected def arrayClass[A](tp: jClass[?]): jClass[Array[A]] =
    java.lang.reflect.Array.newInstance(tp, 0).getClass.asInstanceOf[jClass[Array[A]]]

  @deprecated("use wrap instead", "2.10.0")
  /** Returns a `ClassManifest` for the array type `Array[T]`. */
  def arrayManifest: ClassManifest[Array[T]] =
    ClassManifest.classType[Array[T]](arrayClass[T](runtimeClass), this)

  @deprecated("use wrap.newArray instead", "2.10.0")
  /** Creates a new two-dimensional array `Array[Array[T]]` whose outer dimension
   *  has `len` elements.
   *
   *  @param len the length of the outer array
   *  @return a new `Array[Array[T]]` of length `len`, with unallocated inner arrays
   */
  def newArray2(len: Int): Array[Array[T]] =
    java.lang.reflect.Array.newInstance(arrayClass[T](runtimeClass), len)
      .asInstanceOf[Array[Array[T]]]

  @deprecated("use wrap.wrap.newArray instead", "2.10.0")
  /** Creates a new three-dimensional array `Array[Array[Array[T]]]` whose outer
   *  dimension has `len` elements.
   *
   *  @param len the length of the outer array
   *  @return a new `Array[Array[Array[T]]]` of length `len`, with unallocated inner arrays
   */
  def newArray3(len: Int): Array[Array[Array[T]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[T]](arrayClass[T](runtimeClass)), len)
      .asInstanceOf[Array[Array[Array[T]]]]

  @deprecated("use wrap.wrap.wrap.newArray instead", "2.10.0")
  /** Creates a new four-dimensional array `Array[Array[Array[Array[T]]]]` whose
   *  outer dimension has `len` elements.
   *
   *  @param len the length of the outer array
   *  @return a new `Array[Array[Array[Array[T]]]]` of length `len`, with unallocated inner arrays
   */
  def newArray4(len: Int): Array[Array[Array[Array[T]]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[Array[T]]](arrayClass[Array[T]](arrayClass[T](runtimeClass))), len)
      .asInstanceOf[Array[Array[Array[Array[T]]]]]

  @deprecated("use wrap.wrap.wrap.wrap.newArray instead", "2.10.0")
  /** Creates a new five-dimensional array `Array[Array[Array[Array[Array[T]]]]]`
   *  whose outer dimension has `len` elements.
   *
   *  @param len the length of the outer array
   *  @return a new `Array[Array[Array[Array[Array[T]]]]]` of length `len`, with unallocated inner arrays
   */
  def newArray5(len: Int): Array[Array[Array[Array[Array[T]]]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[Array[Array[T]]]](arrayClass[Array[Array[T]]](arrayClass[Array[T]](arrayClass[T](runtimeClass)))), len)
      .asInstanceOf[Array[Array[Array[Array[Array[T]]]]]]

  @deprecated("create WrappedArray directly instead", "2.10.0")
  /** Creates a new `ArraySeq[T]` backed by a freshly allocated array of length `len`.
   *
   *  @param len the length of the backing array
   *  @return a new `ArraySeq[T]` wrapping an array of length `len`
   */
  def newWrappedArray(len: Int): ArraySeq[T] =
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new ArraySeq.ofRef[T & AnyRef](newArray(len).asInstanceOf[Array[T & AnyRef]]).asInstanceOf[ArraySeq[T]]

  @deprecated("use ArrayBuilder.make(this) instead", "2.10.0")
  /** Returns a new `ArrayBuilder` for arrays with element type `T`. */
  def newArrayBuilder(): ArrayBuilder[T] =
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new ArrayBuilder.ofRef[T & AnyRef]()(using this.asInstanceOf[ClassManifest[T & AnyRef]]).asInstanceOf[ArrayBuilder[T]]

  @deprecated("use scala.reflect.runtime.universe.TypeTag to capture type structure instead", "2.10.0")
  /** Returns the manifests of the type arguments of the type represented by this
   *  manifest; the default implementation returns an empty list.
   */
  def typeArguments: List[OptManifest[?]] = List()

  /** Returns the type-argument portion of this manifest's string representation:
   *  the type arguments in bracketed form, the array component type in brackets,
   *  or the empty string when neither applies.
   */
  protected def argString =
    if (typeArguments.nonEmpty) typeArguments.mkString("[", ", ", "]")
    else if (runtimeClass.isArray) "["+ClassManifest.fromClass(runtimeClass.getComponentType)+"]"
    else ""
}

/** `ClassManifestFactory` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 *
 *  Unlike `ClassManifest`, this factory isn't annotated with a deprecation warning.
 *  This is done to prevent avalanches of deprecation warnings in the code that calls methods with manifests.
 *
 *  In a perfect world, we would just remove the @deprecated annotation from `ClassManifest` the object
 *  and then delete it in 2.11. After all, that object is explicitly marked as internal, so no one should use it.
 *  However a lot of existing libraries disregarded the Scaladoc that comes with `ClassManifest`,
 *  so we need to somehow nudge them into migrating prior to removing stuff out of the blue.
 *  Hence we've introduced this design decision as the lesser of two evils.
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest.*""")
object ClassManifestFactory {
  val Byte    = ManifestFactory.Byte
  val Short   = ManifestFactory.Short
  val Char    = ManifestFactory.Char
  val Int     = ManifestFactory.Int
  val Long    = ManifestFactory.Long
  val Float   = ManifestFactory.Float
  val Double  = ManifestFactory.Double
  val Boolean = ManifestFactory.Boolean
  val Unit    = ManifestFactory.Unit
  val Any     = ManifestFactory.Any
  val Object  = ManifestFactory.Object
  val AnyVal  = ManifestFactory.AnyVal
  val Nothing = ManifestFactory.Nothing
  val Null    = ManifestFactory.Null

  /** Returns the `ClassManifest` corresponding to the runtime class `clazz`, using
   *  the predefined value manifests for the primitive classes and `Unit` for `void`.
   *
   *  @tparam T the type represented by `clazz`
   *  @param clazz the runtime `Class` object to build a manifest for
   */
  def fromClass[T](clazz: jClass[T]): ClassManifest[T] = clazz match {
    case java.lang.Byte.TYPE      => Byte.asInstanceOf[ClassManifest[T]]
    case java.lang.Short.TYPE     => Short.asInstanceOf[ClassManifest[T]]
    case java.lang.Character.TYPE => Char.asInstanceOf[ClassManifest[T]]
    case java.lang.Integer.TYPE   => Int.asInstanceOf[ClassManifest[T]]
    case java.lang.Long.TYPE      => Long.asInstanceOf[ClassManifest[T]]
    case java.lang.Float.TYPE     => Float.asInstanceOf[ClassManifest[T]]
    case java.lang.Double.TYPE    => Double.asInstanceOf[ClassManifest[T]]
    case java.lang.Boolean.TYPE   => Boolean.asInstanceOf[ClassManifest[T]]
    case java.lang.Void.TYPE      => Unit.asInstanceOf[ClassManifest[T]]
    case _                        => classType[T & AnyRef](clazz).asInstanceOf[ClassManifest[T]]
  }

  /** Returns a `Manifest` for the singleton type `value.type`.
   *
   *  @tparam T the type to be represented, typically the singleton type of `value`
   *  @param value the runtime object whose singleton type is represented
   */
  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] = Manifest.singleType(value)

  /** ClassManifest for the class type `clazz`, where `clazz` is
   *  a top-level or static class.
   *  @note This no-prefix, no-arguments case is separate because we
   *       it's called from ScalaRunTime.boxArray itself. If we
   *       pass varargs as arrays into this, we get an infinitely recursive call
   *       to boxArray. (Besides, having a separate case is more efficient)
   *
   *  @tparam T the type represented by the manifest
   *  @param clazz the runtime `Class` object for the type `T`
   */
  def classType[T](clazz: jClass[?]): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** ClassManifest for the class type `clazz[args]`, where `clazz` is
   *  a top-level or static class and `args` are its type arguments 
   *
   *  @tparam T the type represented by the manifest
   *  @param clazz the runtime `Class` object for the type `T`
   *  @param arg1 the manifest for the first type argument of `clazz`, ensuring at least one type argument is provided
   *  @param args the manifests for the remaining type arguments of `clazz`
   */
  def classType[T](clazz: jClass[?], arg1: OptManifest[?], args: OptManifest[?]*): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** ClassManifest for the class type `clazz[args]`, where `clazz` is
   *  a class with non-package prefix type `prefix` and type arguments `args`.
   *
   *  @tparam T the type represented by the manifest
   *  @param prefix the manifest for the non-package prefix type of `clazz`
   *  @param clazz the runtime `Class` object for the type `T`
   *  @param args the manifests for the type arguments of `clazz`
   */
  def classType[T](prefix: OptManifest[?], clazz: jClass[?], args: OptManifest[?]*): ClassManifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  /** Returns a `ClassManifest` for the array type `Array[T]`, given the manifest
   *  `arg` for its element type.
   *
   *  @tparam T the array element type
   *  @param arg the manifest for the element type `T`, or `NoManifest` if unknown
   *  @return the array manifest, or the manifest for `Object` when `arg` is `NoManifest`
   */
  def arrayType[T](arg: OptManifest[?]): ClassManifest[Array[T]] = (arg: @unchecked) match {
    case NoManifest          => Object.asInstanceOf[ClassManifest[Array[T]]]
    case m: ClassManifest[?] => m.asInstanceOf[ClassManifest[T]].arrayManifest
  }

  @SerialVersionUID(1L)
  private class AbstractTypeClassManifest[T](prefix: OptManifest[?], name: String, clazz: jClass[?], args: OptManifest[?]*) extends ClassManifest[T] {
    /** Returns the runtime `Class` used as the erasure of this abstract type. */
    override def runtimeClass = clazz
    override val typeArguments = args.toList
    /** Returns a string representation of this manifest in the form
     *  `prefix#name` followed by its type arguments.
     */
    override def toString() = prefix.toString+"#"+name+argString
  }

  /** ClassManifest for the abstract type `prefix # name`. `upperBound` is not
   *  strictly necessary as it could be obtained by reflection. It was
   *  added so that erasure can be calculated without reflection. 
   *
   *  @tparam T the type represented by the manifest
   *  @param prefix the manifest for the prefix type of the abstract type
   *  @param name the name of the abstract type
   *  @param clazz the runtime `Class` for the upper bound of the abstract type, used for erasure
   *  @param args the manifests for the type arguments of the abstract type (note: currently unused in the implementation)
   */
  def abstractType[T](prefix: OptManifest[?], name: String, clazz: jClass[?], args: OptManifest[?]*): ClassManifest[T] =
    new AbstractTypeClassManifest(prefix, name, clazz)

  /** ClassManifest for the abstract type `prefix # name`. `upperBound` is not
   *  strictly necessary as it could be obtained by reflection. It was
   *  added so that erasure can be calculated without reflection.
   *  todo: remove after next bootstrap
   *
   *  @tparam T the type represented by the manifest
   *  @param prefix the manifest for the prefix type of the abstract type
   *  @param name the name of the abstract type
   *  @param upperbound the `ClassManifest` for the upper bound, whose `runtimeClass` is used for erasure
   *  @param args the manifests for the type arguments of the abstract type (note: currently unused in the implementation)
   */
  def abstractType[T](prefix: OptManifest[?], name: String, upperbound: ClassManifest[?], args: OptManifest[?]*): ClassManifest[T] =
    new AbstractTypeClassManifest(prefix, name, upperbound.runtimeClass)
}

/** Manifest for the class type `clazz[args]`, where `clazz` is
 *  a top-level or static class 
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest""")
@SerialVersionUID(1L)
private class ClassTypeManifest[T](
  prefix: Option[OptManifest[?]],
  val runtimeClass: jClass[?],
  override val typeArguments: List[OptManifest[?]]) extends ClassManifest[T]
{
  /** Returns a string representation of this manifest, combining the optional
   *  prefix, the class name (or `Array`), and its type arguments.
   */
  override def toString() =
    (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
    (if (runtimeClass.isArray) "Array" else runtimeClass.getName) +
    argString
}
