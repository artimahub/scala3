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
import scala.annotation.{implicitNotFound, nowarn}
import scala.collection.mutable.{ArrayBuilder, ArraySeq}

/** A `Manifest[T]` is an opaque descriptor for type T.  Its supported use
 *  is to give access to the erasure of the type as a `Class` instance, as
 *  is necessary for the creation of native `Arrays` if the class is not
 *  known at compile time.
 *
 *  The type-relation operators `<:<` and `=:=` should be considered
 *  approximations only, as there are numerous aspects of type conformance
 *  which are not yet adequately represented in manifests.
 *
 *  Example usages:
 *  ```scala sc:compile
 *    import scala.reflect.Manifest
 *
 *    def arr[T: Manifest]: Array[T] = new Array[T](0)
 *
 *    // Methods manifest and optManifest are in [[scala.Predef]].
 *    def isApproxSubType[T: Manifest, U: Manifest]: Boolean = manifest[T] <:< manifest[U]
 *    val stringsAreAnyRefs = isApproxSubType[List[String], List[AnyRef]]
 *    val stringsAreInts = isApproxSubType[List[String], List[Int]]
 *
 *    def methods[T: Manifest] = manifest[T].runtimeClass.getMethods
 *    def retType[T: Manifest](name: String) =
 *      methods[T].find(_.getName == name).map(_.getGenericReturnType)
 *
 *    val mapValuesReturnType = retType[Map[_, _]]("values")
 *  ```
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
@implicitNotFound(msg = "No Manifest available for ${T}.")
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use scala.reflect.ClassTag (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
/** A descriptor for the type `T` that tracks its type arguments in addition to its
 *  erasure, supporting the approximate type-relation operators `<:<` and `=:=`.
 *
 *  @tparam T the type described by this manifest
 */
trait Manifest[T] extends ClassManifest[T] with Equals {
  /** Returns the manifests for the type arguments of the type `T`, or `Nil` if it has none. */
  override def typeArguments: List[Manifest[?]] = Nil

  /** Returns the manifest for `Array[T]`. */
  override def arrayManifest: Manifest[Array[T]] =
    Manifest.classType[Array[T]](arrayClass[T](runtimeClass), this)

  /** Tests whether `that` may be compared for equality with this manifest, which
   *  holds when `that` is itself a `Manifest`.
   *
   *  @param that the value being tested for possible equality with this manifest
   *  @return `true` if `that` is a `Manifest`, `false` otherwise
   */
  override def canEqual(that: Any): Boolean = that match {
    case _: Manifest[?]   => true
    case _                => false
  }
  /** Note: testing for erasure here is important, as it is many times
   *  faster than <:< and rules out most comparisons.
   *
   *  @param that the object to compare for equality (only `Manifest` instances can be equal)
   */
  override def equals(that: Any): Boolean = that match {
    case m: Manifest[?] => (m canEqual this) && (this.runtimeClass == m.runtimeClass) && (this <:< m) && (m <:< this)
    case _              => false
  }
  /** Returns a hash code derived from the wrapped `runtimeClass`. */
  override def hashCode() = this.runtimeClass.##
}

/** The object `Manifest` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 */
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use scala.reflect.ClassTag (to capture erasures), scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
object Manifest {
  /* Forward all the public members of ManifestFactory, since this object used
   * to be a `private val Manifest = ManifestFactory` in the package object. It
   * was moved here because it needs to be in the same file as `trait Manifest`
   * defined above.
   */

  /** Returns the list of manifests for the primitive value types (`Byte`, `Short`, `Char`,
   *  `Int`, `Long`, `Float`, `Double`, `Boolean`, and `Unit`).
   */
  def valueManifests: List[AnyValManifest[?]] =
    ManifestFactory.valueManifests

  val Byte: ManifestFactory.ByteManifest = ManifestFactory.Byte
  val Short: ManifestFactory.ShortManifest = ManifestFactory.Short
  val Char: ManifestFactory.CharManifest = ManifestFactory.Char
  val Int: ManifestFactory.IntManifest = ManifestFactory.Int
  val Long: ManifestFactory.LongManifest = ManifestFactory.Long
  val Float: ManifestFactory.FloatManifest = ManifestFactory.Float
  val Double: ManifestFactory.DoubleManifest = ManifestFactory.Double
  val Boolean: ManifestFactory.BooleanManifest = ManifestFactory.Boolean
  val Unit: ManifestFactory.UnitManifest = ManifestFactory.Unit

  val Any: Manifest[scala.Any] = ManifestFactory.Any
  val Object: Manifest[java.lang.Object] = ManifestFactory.Object
  val AnyRef: Manifest[scala.AnyRef] = ManifestFactory.AnyRef
  val AnyVal: Manifest[scala.AnyVal] = ManifestFactory.AnyVal
  val Null: Manifest[scala.Null] = ManifestFactory.Null
  val Nothing: Manifest[scala.Nothing] = ManifestFactory.Nothing

  /** Manifest for the singleton type `value.type`.
   *
   *  @tparam T the type to be represented, typically inferred as the singleton type of `value`
   *  @param value the runtime object whose singleton type is represented
   */
  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] =
    ManifestFactory.singleType[T](value)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a top-level or static class.
   *  @note This no-prefix, no-arguments case is separate because we
   *       it's called from ScalaRunTime.boxArray itself. If we
   *       pass varargs as arrays into this, we get an infinitely recursive call
   *       to boxArray. (Besides, having a separate case is more efficient)
   *
   *  @tparam T the type represented by this manifest
   *  @param clazz the runtime `Class` for the type `T`
   */
  def classType[T](clazz: Predef.Class[?]): Manifest[T] =
    ManifestFactory.classType[T](clazz)

  /** Manifest for the class type `clazz`, where `clazz` is
   *  a top-level or static class and args are its type arguments.
   *
   *  @tparam T the type represented by this manifest
   *  @param clazz the runtime `Class` for the type `T`
   *  @param arg1 the manifest for the first type argument (required to ensure at least one type argument)
   *  @param args the manifests for the remaining type arguments
   */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[?], args: Manifest[?]*): Manifest[T] =
    ManifestFactory.classType[T](clazz, arg1, args*)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a class with non-package prefix type `prefix` and type arguments `args`.
   *
   *  @tparam T the type represented by this manifest
   *  @param prefix the manifest for the non-package prefix type
   *  @param clazz the runtime `Class` for the type `T`
   *  @param args the manifests for the type arguments
   */
  def classType[T](prefix: Manifest[?], clazz: Predef.Class[?], args: Manifest[?]*): Manifest[T] =
    ManifestFactory.classType[T](prefix, clazz, args*)

  /** Returns the manifest for `Array[T]`, given the manifest `arg` for the element type `T`.
   *
   *  @tparam T the array element type
   *  @param arg the manifest for the element type `T`
   */
  def arrayType[T](arg: Manifest[?]): Manifest[Array[T]] =
    ManifestFactory.arrayType[T](arg)

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
   *  strictly necessary as it could be obtained by reflection. It was
   *  added so that erasure can be calculated without reflection.
   *
   *  @tparam T the type represented by this manifest
   *  @param prefix the manifest for the type containing this abstract type member
   *  @param name the name of the abstract type member
   *  @param upperBound the runtime `Class` of the upper bound, used to compute erasure without reflection
   *  @param args the manifests for the type arguments
   */
  def abstractType[T](prefix: Manifest[?], name: String, upperBound: Predef.Class[?], args: Manifest[?]*): Manifest[T] =
    ManifestFactory.abstractType[T](prefix, name, upperBound, args*)

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
   *
   *  @tparam T the type represented by this manifest
   *  @param lowerBound the manifest for the lower bound `L` of the wildcard
   *  @param upperBound the manifest for the upper bound `U` of the wildcard
   */
  def wildcardType[T](lowerBound: Manifest[?], upperBound: Manifest[?]): Manifest[T] =
    ManifestFactory.wildcardType[T](lowerBound, upperBound)

  /** Manifest for the intersection type `parents_0 with ... with parents_n`.
   *
   *  @tparam T the type represented by this manifest
   *  @param parents the manifests for each type in the intersection
   */
  def intersectionType[T](parents: Manifest[?]*): Manifest[T] =
    ManifestFactory.intersectionType[T](parents*)

}

// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use type tags and manually check the corresponding class or type instead", "2.10.0")
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
@SerialVersionUID(1L)
/** A `Manifest` for one of the primitive value types, such as `Int` or `Boolean`.
 *
 *  @tparam T the primitive value type represented by this manifest
 *  @param toString the string representation of this manifest, namely the name of the value type
 */
abstract class AnyValManifest[T <: AnyVal](override val toString: String) extends Manifest[T] with Equals {
  override def <:<(that: ClassManifest[?]): Boolean =
    (that eq this) || (that eq Manifest.Any) || (that eq Manifest.AnyVal)
  /** Tests whether `other` may be compared for equality with this manifest, which
   *  holds when `other` is itself an `AnyValManifest`.
   *
   *  @param other the value being tested for possible equality with this manifest
   *  @return `true` if `other` is an `AnyValManifest`, `false` otherwise
   */
  override def canEqual(other: Any) = other match {
    case _: AnyValManifest[?] => true
    case _                    => false
  }
  /** Tests whether `that` is this same manifest instance, since value-type manifests are singletons.
   *
   *  @param that the value to compare with this manifest
   *  @return `true` if `that` is this exact manifest instance, `false` otherwise
   */
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  /** Returns an identity-based hash code for this singleton manifest. */
  override def hashCode = System.identityHashCode(this)
}

/** `ManifestFactory` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 *
 *  Unlike `Manifest`, this factory isn't annotated with a deprecation warning.
 *  This is done to prevent avalanches of deprecation warnings in the code that calls methods with manifests.
 *  Why so complicated? Read up the comments for `ClassManifestFactory`.
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
object ManifestFactory {
  /** Returns the list of manifests for the primitive value types (`Byte`, `Short`, `Char`,
   *  `Int`, `Long`, `Float`, `Double`, `Boolean`, and `Unit`).
   */
  def valueManifests: List[AnyValManifest[?]] =
    List(Byte, Short, Char, Int, Long, Float, Double, Boolean, Unit)

  @SerialVersionUID(1L)
  final private[reflect] class ByteManifest extends AnyValManifest[scala.Byte]("Byte") {
    /** Returns the runtime class for the primitive `Byte` type, namely `java.lang.Byte.TYPE`. */
    def runtimeClass: Class[java.lang.Byte] = java.lang.Byte.TYPE
    @inline override def newArray(len: Int): Array[Byte] = new Array[Byte](len)
    /** Creates a new `ArraySeq[Byte]` backed by a freshly allocated `Array[Byte]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Byte]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Byte] = new ArraySeq.ofByte(new Array[Byte](len))
    /** Returns a new `ArrayBuilder` for `Byte` elements. */
    override def newArrayBuilder(): ArrayBuilder[Byte] = new ArrayBuilder.ofByte()
    /** Matches `x` against the `Byte` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Byte`
     *  @return `Some(x)` if `x` is a `Byte`, `None` otherwise
     */
    override def unapply(x: Any): Option[Byte] = {
      x match {
        case d: Byte => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Byte
  }
  val Byte: ByteManifest = new ByteManifest

  @SerialVersionUID(1L)
  final private[reflect] class ShortManifest extends AnyValManifest[scala.Short]("Short") {
    /** Returns the runtime class for the primitive `Short` type, namely `java.lang.Short.TYPE`. */
    def runtimeClass: Class[java.lang.Short] = java.lang.Short.TYPE
    @inline override def newArray(len: Int): Array[Short] = new Array[Short](len)
    /** Creates a new `ArraySeq[Short]` backed by a freshly allocated `Array[Short]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Short]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Short] = new ArraySeq.ofShort(new Array[Short](len))
    /** Returns a new `ArrayBuilder` for `Short` elements. */
    override def newArrayBuilder(): ArrayBuilder[Short] = new ArrayBuilder.ofShort()
    /** Matches `x` against the `Short` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Short`
     *  @return `Some(x)` if `x` is a `Short`, `None` otherwise
     */
    override def unapply(x: Any): Option[Short] = {
      x match {
        case d: Short => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Short
  }
  val Short: ShortManifest = new ShortManifest

  @SerialVersionUID(1L)
  final private[reflect] class CharManifest extends AnyValManifest[scala.Char]("Char") {
    /** Returns the runtime class for the primitive `Char` type, namely `java.lang.Character.TYPE`. */
    def runtimeClass: Class[java.lang.Character] = java.lang.Character.TYPE
    @inline override def newArray(len: Int): Array[Char] = new Array[Char](len)
    /** Creates a new `ArraySeq[Char]` backed by a freshly allocated `Array[Char]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Char]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Char] = new ArraySeq.ofChar(new Array[Char](len))
    /** Returns a new `ArrayBuilder` for `Char` elements. */
    override def newArrayBuilder(): ArrayBuilder[Char] = new ArrayBuilder.ofChar()
    /** Matches `x` against the `Char` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Char`
     *  @return `Some(x)` if `x` is a `Char`, `None` otherwise
     */
    override def unapply(x: Any): Option[Char] = {
      x match {
        case d: Char => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Char
  }
  val Char: CharManifest = new CharManifest

  @SerialVersionUID(1L)
  final private[reflect] class IntManifest extends AnyValManifest[scala.Int]("Int") {
    /** Returns the runtime class for the primitive `Int` type, namely `java.lang.Integer.TYPE`. */
    def runtimeClass: Class[java.lang.Integer] = java.lang.Integer.TYPE
    @inline override def newArray(len: Int): Array[Int] = new Array[Int](len)
    /** Creates a new `ArraySeq[Int]` backed by a freshly allocated `Array[Int]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Int]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Int] = new ArraySeq.ofInt(new Array[Int](len))
    /** Returns a new `ArrayBuilder` for `Int` elements. */
    override def newArrayBuilder(): ArrayBuilder[Int] = new ArrayBuilder.ofInt()
    /** Matches `x` against the `Int` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Int`
     *  @return `Some(x)` if `x` is an `Int`, `None` otherwise
     */
    override def unapply(x: Any): Option[Int] = {
      x match {
        case d: Int => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Int
  }
  val Int: IntManifest = new IntManifest

  @SerialVersionUID(1L)
  final private[reflect] class LongManifest extends AnyValManifest[scala.Long]("Long") {
    /** Returns the runtime class for the primitive `Long` type, namely `java.lang.Long.TYPE`. */
    def runtimeClass: Class[java.lang.Long] = java.lang.Long.TYPE
    @inline override def newArray(len: Int): Array[Long] = new Array[Long](len)
    /** Creates a new `ArraySeq[Long]` backed by a freshly allocated `Array[Long]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Long]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Long] = new ArraySeq.ofLong(new Array[Long](len))
    /** Returns a new `ArrayBuilder` for `Long` elements. */
    override def newArrayBuilder(): ArrayBuilder[Long] = new ArrayBuilder.ofLong()
    /** Matches `x` against the `Long` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Long`
     *  @return `Some(x)` if `x` is a `Long`, `None` otherwise
     */
    override def unapply(x: Any): Option[Long] = {
      x match {
        case d: Long => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Long
  }
  val Long: LongManifest = new LongManifest

  @SerialVersionUID(1L)
  final private[reflect] class FloatManifest extends AnyValManifest[scala.Float]("Float") {
    /** Returns the runtime class for the primitive `Float` type, namely `java.lang.Float.TYPE`. */
    def runtimeClass: Class[java.lang.Float] = java.lang.Float.TYPE
    @inline override def newArray(len: Int): Array[Float] = new Array[Float](len)
    /** Creates a new `ArraySeq[Float]` backed by a freshly allocated `Array[Float]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Float]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Float] = new ArraySeq.ofFloat(new Array[Float](len))
    /** Returns a new `ArrayBuilder` for `Float` elements. */
    override def newArrayBuilder(): ArrayBuilder[Float] = new ArrayBuilder.ofFloat()
    /** Matches `x` against the `Float` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Float`
     *  @return `Some(x)` if `x` is a `Float`, `None` otherwise
     */
    override def unapply(x: Any): Option[Float] = {
      x match {
        case d: Float => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Float
  }
  val Float: FloatManifest = new FloatManifest

  @SerialVersionUID(1L)
  final private[reflect] class DoubleManifest extends AnyValManifest[scala.Double]("Double") {
    /** Returns the runtime class for the primitive `Double` type, namely `java.lang.Double.TYPE`. */
    def runtimeClass: Class[java.lang.Double] = java.lang.Double.TYPE
    @inline override def newArray(len: Int): Array[Double] = new Array[Double](len)
    /** Creates a new `ArraySeq[Double]` backed by a freshly allocated `Array[Double]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Double]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Double] = new ArraySeq.ofDouble(new Array[Double](len))
    /** Returns a new `ArrayBuilder` for `Double` elements. */
    override def newArrayBuilder(): ArrayBuilder[Double] = new ArrayBuilder.ofDouble()

    /** Matches `x` against the `Double` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Double`
     *  @return `Some(x)` if `x` is a `Double`, `None` otherwise
     */
    override def unapply(x: Any): Option[Double] = {
      x match {
        case d: Double => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Double
  }
  val Double: DoubleManifest = new DoubleManifest

  @SerialVersionUID(1L)
  final private[reflect] class BooleanManifest extends AnyValManifest[scala.Boolean]("Boolean") {
    /** Returns the runtime class for the primitive `Boolean` type, namely `java.lang.Boolean.TYPE`. */
    def runtimeClass: Class[java.lang.Boolean] = java.lang.Boolean.TYPE
    @inline override def newArray(len: Int): Array[Boolean] = new Array[Boolean](len)
    /** Creates a new `ArraySeq[Boolean]` backed by a freshly allocated `Array[Boolean]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Boolean]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Boolean] = new ArraySeq.ofBoolean(new Array[Boolean](len))
    /** Returns a new `ArrayBuilder` for `Boolean` elements. */
    override def newArrayBuilder(): ArrayBuilder[Boolean] = new ArrayBuilder.ofBoolean()
    /** Matches `x` against the `Boolean` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Boolean`
     *  @return `Some(x)` if `x` is a `Boolean`, `None` otherwise
     */
    override def unapply(x: Any): Option[Boolean] = {
      x match {
        case d: Boolean => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Boolean
  }
  val Boolean: BooleanManifest = new BooleanManifest

  @SerialVersionUID(1L)
  final private[reflect] class UnitManifest extends AnyValManifest[scala.Unit]("Unit") {
    /** Returns the runtime class for the `Unit` type, namely `java.lang.Void.TYPE`. */
    def runtimeClass: Class[java.lang.Void] = java.lang.Void.TYPE
    @inline override def newArray(len: Int): Array[Unit] = new Array[Unit](len)
    /** Creates a new `ArraySeq[Unit]` backed by a freshly allocated `Array[Unit]` of length `len`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq[Unit]` wrapping an array of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Unit] = new ArraySeq.ofUnit(new Array[Unit](len))
    /** Returns a new `ArrayBuilder` for `Unit` elements. */
    override def newArrayBuilder(): ArrayBuilder[Unit] = new ArrayBuilder.ofUnit()
    /** Returns the runtime `Class` for arrays of element type `T`, using `Array[BoxedUnit]` when
     *  `tp` is the `Unit` runtime class and delegating to the default implementation otherwise.
     *
     *  @tparam T the array element type
     *  @param tp the runtime `Class` of the array element type
     */
    override protected def arrayClass[T](tp: Class[?]): Class[Array[T]] =
      if (tp eq runtimeClass) classOf[Array[scala.runtime.BoxedUnit]].asInstanceOf[Class[Array[T]]]
      else super.arrayClass(tp)
    /** Matches `x` against the `Unit` type, allowing this manifest to be used as an extractor.
     *
     *  @param x the value to match against `Unit`
     *  @return `Some(x)` if `x` is a `Unit`, `None` otherwise
     */
    override def unapply(x: Any): Option[Unit] = {
      x match {
        case d: Unit => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Unit
  }
  val Unit: UnitManifest = new UnitManifest

  private val ObjectTYPE = classOf[java.lang.Object]
  private val NothingTYPE = classOf[scala.runtime.Nothing$]
  private val NullTYPE = classOf[scala.runtime.Null$]

  @SerialVersionUID(1L)
  final private class AnyManifest extends PhantomManifest[scala.Any](ObjectTYPE, "Any") {
    /** Returns a new `Array[Any]` of length `len`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Any](len)
    override def <:<(that: ClassManifest[?]): Boolean = (that eq this)
    private def readResolve(): Any = Manifest.Any
  }
  val Any: Manifest[scala.Any] = new AnyManifest

  @SerialVersionUID(1L)
  final private class ObjectManifest extends PhantomManifest[java.lang.Object](ObjectTYPE, "Object") {
    /** Returns a new `Array[Object]` of length `len`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[java.lang.Object](len)
    override def <:<(that: ClassManifest[?]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.Object
  }
  val Object: Manifest[java.lang.Object] = new ObjectManifest

  val AnyRef: Manifest[scala.AnyRef] = Object.asInstanceOf[Manifest[scala.AnyRef]]

  @SerialVersionUID(1L)
  final private class AnyValPhantomManifest extends PhantomManifest[scala.AnyVal](ObjectTYPE, "AnyVal") {
    /** Returns a new `Array[AnyVal]` of length `len`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.AnyVal](len)
    override def <:<(that: ClassManifest[?]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.AnyVal
  }
  val AnyVal: Manifest[scala.AnyVal] = new AnyValPhantomManifest

  @SerialVersionUID(1L)
  final private class NullManifest extends PhantomManifest[scala.Null](NullTYPE, "Null") {
    /** Returns a new `Array[Null]` of length `len`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Null](len)
    override def <:<(that: ClassManifest[?]): Boolean =
      (that ne null) && (that ne Nothing) && !(that <:< AnyVal)
    private def readResolve(): Any = Manifest.Null
  }
  val Null: Manifest[scala.Null] = new NullManifest

  @SerialVersionUID(1L)
  final private class NothingManifest extends PhantomManifest[scala.Nothing](NothingTYPE, "Nothing") {
    /** Returns a new `Array[Nothing]` of length `len`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Nothing](len)
    override def <:<(that: ClassManifest[?]): Boolean = (that ne null)
    private def readResolve(): Any = Manifest.Nothing
  }
  val Nothing: Manifest[scala.Nothing] = new NothingManifest

  @SerialVersionUID(1L)
  final private class SingletonTypeManifest[T <: AnyRef](value: AnyRef) extends Manifest[T] {
    lazy val runtimeClass: Class[? <: AnyRef] = value.getClass
    override lazy val toString = value.toString + ".type"
  }

  /** Manifest for the singleton type `value.type`.
   *
   *  @tparam T the type to be represented, typically inferred as the singleton type of `value`
   *  @param value the runtime object whose singleton type is represented
   */
  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] =
    new SingletonTypeManifest[T](value)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a top-level or static class.
   *  @note This no-prefix, no-arguments case is separate because we
   *       it's called from ScalaRunTime.boxArray itself. If we
   *       pass varargs as arrays into this, we get an infinitely recursive call
   *       to boxArray. (Besides, having a separate case is more efficient)
   *
   *  @tparam T the type represented by this manifest
   *  @param clazz the runtime `Class` for the type `T`
   */
  def classType[T](clazz: Predef.Class[?]): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** Manifest for the class type `clazz`, where `clazz` is
   *  a top-level or static class and args are its type arguments.
   *
   *  @tparam T the type represented by this manifest
   *  @param clazz the runtime `Class` for the type `T`
   *  @param arg1 the manifest for the first type argument (required to ensure at least one type argument)
   *  @param args the manifests for the remaining type arguments
   */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[?], args: Manifest[?]*): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a class with non-package prefix type `prefix` and type arguments `args`.
   *
   *  @tparam T the type represented by this manifest
   *  @param prefix the manifest for the non-package prefix type
   *  @param clazz the runtime `Class` for the type `T`
   *  @param args the manifests for the type arguments
   */
  def classType[T](prefix: Manifest[?], clazz: Predef.Class[?], args: Manifest[?]*): Manifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  @SerialVersionUID(1L)
  private abstract class PhantomManifest[T](_runtimeClass: Predef.Class[?],
                                            override val toString: String) extends ClassTypeManifest[T](None, _runtimeClass, Nil) {
    /** Tests whether `that` is this same manifest instance, since phantom manifests are singletons.
     *
     *  @param that the value to compare with this manifest
     *  @return `true` if `that` is this exact manifest instance, `false` otherwise
     */
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    /** Returns an identity-based hash code for this singleton manifest. */
    override def hashCode = System.identityHashCode(this)
  }

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a top-level or static class.
   */
  @SerialVersionUID(1L)
  private class ClassTypeManifest[T](prefix: Option[Manifest[?]],
                                     val runtimeClass: Predef.Class[?],
                                     override val typeArguments: List[Manifest[?]]) extends Manifest[T] {
    /** Returns the string representation of this manifest, combining its optional prefix,
     *  class name, and type arguments.
     */
    override def toString() =
      (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
      (if (runtimeClass.isArray) "Array" else runtimeClass.getName) +
      argString
   }

  /** Returns the manifest for `Array[T]`, given the manifest `arg` for the element type `T`.
   *
   *  @tparam T the array element type
   *  @param arg the manifest for the element type `T`
   */
  def arrayType[T](arg: Manifest[?]): Manifest[Array[T]] =
    arg.asInstanceOf[Manifest[T]].arrayManifest

  @SerialVersionUID(1L)
  private class AbstractTypeManifest[T](prefix: Manifest[?], name: String, upperBound: Predef.Class[?], args: scala.collection.Seq[Manifest[?]]) extends Manifest[T] {
    /** Returns the runtime `Class` of the abstract type's upper bound, which serves as its erasure. */
    def runtimeClass = upperBound
    override val typeArguments = args.toList
    /** Returns the string representation `prefix#name` followed by any type arguments. */
    override def toString() = prefix.toString+"#"+name+argString
  }

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
   *  strictly necessary as it could be obtained by reflection. It was
   *  added so that erasure can be calculated without reflection.
   *
   *  @tparam T the type represented by this manifest
   *  @param prefix the manifest for the type containing this abstract type member
   *  @param name the name of the abstract type member
   *  @param upperBound the runtime `Class` of the upper bound, used to compute erasure without reflection
   *  @param args the manifests for the type arguments
   */
  def abstractType[T](prefix: Manifest[?], name: String, upperBound: Predef.Class[?], args: Manifest[?]*): Manifest[T] =
    new AbstractTypeManifest[T](prefix, name, upperBound, args)

  @SerialVersionUID(1L)
  private class WildcardManifest[T](lowerBound: Manifest[?], upperBound: Manifest[?]) extends Manifest[T] {
    /** Returns the runtime `Class` of the wildcard's upper bound, which serves as its erasure. */
    def runtimeClass = upperBound.runtimeClass
    /** Returns the string representation of the wildcard, such as `_ >: L <: U`, omitting bounds that are `Nothing`. */
    override def toString() =
      "_" +
        (if (lowerBound eq Nothing) "" else " >: "+lowerBound) +
        (if (upperBound eq Nothing) "" else " <: "+upperBound)
  }

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
   *
   *  @tparam T the type represented by this manifest
   *  @param lowerBound the manifest for the lower bound `L` of the wildcard
   *  @param upperBound the manifest for the upper bound `U` of the wildcard
   */
  def wildcardType[T](lowerBound: Manifest[?], upperBound: Manifest[?]): Manifest[T] =
    new WildcardManifest[T](lowerBound, upperBound)

  @SerialVersionUID(1L)
  private class IntersectionTypeManifest[T](parents: Array[Manifest[?]]) extends Manifest[T] {
    // We use an `Array` instead of a `Seq` for `parents` to avoid cyclic dependencies during deserialization
    // which can cause serialization proxies to leak and cause a ClassCastException.
    /** Returns the runtime `Class` of the first parent, which serves as the erasure of the intersection type. */
    def runtimeClass = parents(0).runtimeClass
    /** Returns the string representation of the intersection, its parent manifests joined by `with`. */
    override def toString() = parents.mkString(" with ")
  }

  /** Manifest for the intersection type `parents_0 with ... with parents_n`.
   *
   *  @tparam T the type represented by this manifest
   *  @param parents the manifests for each type in the intersection
   */
  def intersectionType[T](parents: Manifest[?]*): Manifest[T] =
    new IntersectionTypeManifest[T](parents.toArray)
}
