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
package collection
package convert

import scala.language.`2.13`
import language.experimental.captureChecking

import java.util.{concurrent => juc}
import java.util.{NavigableMap}
import java.{lang => jl, util => ju}

import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.chaining._
import scala.util.control.ControlThrowable

/** Wrappers for exposing Scala collections as Java collections and vice-versa. */
@SerialVersionUID(3L)
// not private[convert] because `WeakHashMap` uses JMapWrapper
private[collection] object JavaCollectionWrappers extends Serializable {
  /** Wraps a Scala `Iterator`, exposing it as both a Java `Iterator` and a
   *  Java `Enumeration`.
   *
   *  Both interfaces delegate to the wrapped iterator, so advancing this
   *  wrapper through either interface advances the underlying iterator.
   *
   *  @tparam A the type of the iterator's elements
   *  @param underlying the wrapped Scala iterator
   */
  @SerialVersionUID(3L)
  class IteratorWrapper[A](val underlying: Iterator[A]^) extends ju.Iterator[A] with ju.Enumeration[A] with Serializable {
    def hasNext = underlying.hasNext
    def next(): A = underlying.next()
    def hasMoreElements = underlying.hasNext
    def nextElement(): A = underlying.next()
    override def remove(): Nothing = throw new UnsupportedOperationException
    override def equals(other: Any): Boolean = other match {
      case that: IteratorWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Iterator` as a Scala `Iterator`.
   *
   *  Delegates to the wrapped iterator, so advancing this iterator advances
   *  the underlying one and vice versa.
   *
   *  @tparam A the type of the iterator's elements
   *  @param underlying the wrapped Java iterator
   */
  @SerialVersionUID(3L)
  class JIteratorWrapper[A](val underlying: ju.Iterator[A]) extends AbstractIterator[A] with Serializable {
    def hasNext = underlying.hasNext
    def next(): A = underlying.next
    override def equals(other: Any): Boolean = other match {
      case that: JIteratorWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Enumeration` as a Scala `Iterator`.
   *
   *  Delegates to the wrapped enumeration, so advancing this iterator
   *  advances the underlying enumeration and vice versa.
   *
   *  @tparam A the type of the enumeration's elements
   *  @param underlying the wrapped Java enumeration
   */
  @SerialVersionUID(3L)
  class JEnumerationWrapper[A](val underlying: ju.Enumeration[A]) extends AbstractIterator[A] with Serializable {
    def hasNext = underlying.hasMoreElements
    def next(): A = underlying.nextElement
    override def equals(other: Any): Boolean = other match {
      case that: JEnumerationWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Common implementations of `java.util.Collection` methods for wrappers
   *  that expose a Scala `Iterable` as a Java collection, all delegating to
   *  the wrapped collection.
   *
   *  @tparam A the type of the collection's elements
   */
  trait IterableWrapperTrait[A] extends ju.AbstractCollection[A] {
    val underlying: Iterable[A]^
    def size = underlying.size
    override def iterator: IteratorWrapper[A]^{this} = new IteratorWrapper(underlying.iterator)
    override def isEmpty = underlying.isEmpty
  }

  /** Wraps a Scala `Iterable` as a Java `Collection`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped
   *  collection at all times. Modification through the Java interface is not
   *  supported; the mutating methods inherited from
   *  `java.util.AbstractCollection` throw `UnsupportedOperationException`.
   *
   *  @tparam A the type of the collection's elements
   *  @param underlying the wrapped Scala collection
   */
  @SerialVersionUID(3L)
  class IterableWrapper[A](val underlying: Iterable[A]^) extends ju.AbstractCollection[A] with IterableWrapperTrait[A] with Serializable {
    override def equals(other: Any): Boolean = other match {
      case that: IterableWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Iterable` as a Scala `Iterable`.
   *
   *  The wrapper is a view: iterating it iterates the wrapped iterable, so it
   *  reflects the wrapped iterable's contents at all times.
   *
   *  @tparam A the type of the iterable's elements
   *  @param underlying the wrapped Java iterable
   */
  @SerialVersionUID(3L)
  class JIterableWrapper[A](val underlying: jl.Iterable[A])
    extends AbstractIterable[A]
      with StrictOptimizedIterableOps[A, Iterable, Iterable[A]]
      with Serializable {
    def iterator = underlying.iterator.asScala
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    override def isEmpty: Boolean = !underlying.iterator().hasNext
    override def equals(other: Any): Boolean = other match {
      case that: JIterableWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Collection` as a Scala `Iterable`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped
   *  collection at all times.
   *
   *  @tparam A the type of the collection's elements
   *  @param underlying the wrapped Java collection
   */
  @SerialVersionUID(3L)
  class JCollectionWrapper[A](val underlying: ju.Collection[A])
    extends AbstractIterable[A]
      with StrictOptimizedIterableOps[A, Iterable, Iterable[A]]
      with Serializable {
    def iterator: Iterator[A] = underlying.iterator.asScala
    override def size = underlying.size
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    override def isEmpty = underlying.isEmpty
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    override def equals(other: Any): Boolean = other match {
      case that: JCollectionWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Scala `Seq` as a Java `List`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped sequence
   *  at all times. Modification through the Java interface is not supported;
   *  the mutating methods inherited from `java.util.AbstractList` throw
   *  `UnsupportedOperationException`.
   *
   *  @tparam A the type of the sequence's elements
   *  @param underlying the wrapped Scala sequence
   */
  @SerialVersionUID(3L)
  class SeqWrapper[A](val underlying: Seq[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    def get(i: Int): A = underlying(i)
  }

  /** Wraps a Scala `mutable.Seq` as a Java `List`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped sequence
   *  at all times, and replacing an element with `set` writes through to the
   *  wrapped sequence. Structural modification (adding or removing elements)
   *  is not supported; those methods, inherited from `java.util.AbstractList`,
   *  throw `UnsupportedOperationException`.
   *
   *  @tparam A the type of the sequence's elements
   *  @param underlying the wrapped Scala sequence
   */
  @SerialVersionUID(3L)
  class MutableSeqWrapper[A](val underlying: mutable.Seq[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    def get(i: Int): A = underlying(i)
    override def set(i: Int, elem: A): A = {
      val p = underlying(i)
      underlying(i) = elem
      p
    }
  }

  /** Wraps a Scala `mutable.Buffer` as a Java `List`.
   *
   *  The wrapper is a view: changes made through either interface are visible
   *  through the other. Supports element access and replacement, appending,
   *  and removal by index.
   *
   *  @tparam A the type of the buffer's elements
   *  @param underlying the wrapped Scala buffer
   */
  @SerialVersionUID(3L)
  class MutableBufferWrapper[A](val underlying: mutable.Buffer[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    def get(i: Int): A = underlying(i)
    override def set(i: Int, elem: A): A = { val p = underlying(i); underlying(i) = elem; p }
    override def add(elem: A) = { underlying += elem; true }
    override def remove(i: Int): A = underlying remove i
  }

  /** Wraps a Java `List` as a Scala `mutable.Buffer`.
   *
   *  The wrapper is a view: changes made through either interface are visible
   *  through the other.
   *
   *  @tparam A the type of the list's elements
   *  @param underlying the wrapped Java list
   */
  @SerialVersionUID(3L)
  class JListWrapper[A](val underlying: ju.List[A])
    extends mutable.AbstractBuffer[A]
      with SeqOps[A, mutable.Buffer, mutable.Buffer[A]]
      with StrictOptimizedSeqOps[A, mutable.Buffer, mutable.Buffer[A]]
      with IterableFactoryDefaults[A, mutable.Buffer]
      with Serializable {
    def length = underlying.size
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    override def isEmpty = underlying.isEmpty
    override def iterator: Iterator[A] = underlying.iterator.asScala
    def apply(i: Int): A = underlying.get(i)
    def update(i: Int, elem: A) = underlying.set(i, elem)
    def prepend(elem: A) = { underlying.subList(0, 0).add(elem); this }
    def addOne(elem: A): this.type = { underlying.add(elem); this }
    def insert(idx: Int,elem: A): Unit = underlying.subList(0, idx).add(elem)
    def insertAll(i: Int, elems: IterableOnce[A]^) = {
      val ins = underlying.subList(0, i)
      elems.iterator.foreach(ins.add(_))
    }
    def remove(i: Int) = underlying.remove(i)
    def clear() = underlying.clear()
    // Note: Clone cannot just call underlying.clone because in Java, only specific collections
    // expose clone methods.  Generically, they're protected.
    override def clone(): JListWrapper[A] = new JListWrapper(new ju.ArrayList[A](underlying))
    def patchInPlace(from: Int, patch: scala.collection.IterableOnce[A]^, replaced: Int): this.type = {
      remove(from, replaced)
      insertAll(from, patch)
      this
    }
    def remove(from: Int, n: Int): Unit = underlying.subList(from, from+n).clear()
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    override def subtractOne(elem: A): this.type = { underlying.remove(elem.asInstanceOf[AnyRef]); this }
  }

  /** Wraps a Scala `Set` as a Java `Set`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped set at
   *  all times. Adding elements through the Java interface is not supported;
   *  the iterator's `remove` works only when the wrapped set is a
   *  `mutable.Set`.
   *
   *  @tparam A the type of the set's elements
   *  @param underlying the wrapped Scala set
   */
  @SerialVersionUID(3L)
  class SetWrapper[A](underlying: Set[A]) extends ju.AbstractSet[A] with Serializable { self =>
    // Note various overrides to avoid performance gotchas.
    override def contains(o: Object): Boolean = {
      try { underlying.contains(o.asInstanceOf[A]) }
      catch { case cce: ClassCastException => false }
    }
    override def isEmpty = underlying.isEmpty
    def size = underlying.size
    def iterator: ju.Iterator[A] = new ju.Iterator[A] {
      val ui = underlying.iterator
      var prev: Option[A] = None
      def hasNext = ui.hasNext
      def next: A = { val e = ui.next(); prev = Some(e); e }
      override def remove() = prev match {
        case Some(e) =>
          underlying match {
            case ms: mutable.Set[a] =>
              ms remove e
              prev = None
            case _ =>
              throw new UnsupportedOperationException("remove")
          }
        case _ =>
          throw new IllegalStateException("next must be called at least once before remove")
      }
    }
  }

  /** Wraps a Scala `mutable.Set` as a Java `Set`.
   *
   *  The wrapper is a view supporting addition, removal, and clearing:
   *  changes made through either interface are visible through the other.
   *
   *  @tparam A the type of the set's elements
   *  @param underlying the wrapped Scala set
   */
  @SerialVersionUID(3L)
  class MutableSetWrapper[A](val underlying: mutable.Set[A]) extends SetWrapper[A](underlying) with Serializable {
    override def add(elem: A) = {
      val sz = underlying.size
      underlying += elem
      sz < underlying.size
    }
    override def remove(elem: AnyRef) =
      try underlying.remove(elem.asInstanceOf[A])
      catch { case ex: ClassCastException => false }
    override def clear() = underlying.clear()
  }

  /** Wraps a Java `Set` as a Scala `mutable.Set`.
   *
   *  The wrapper is a view: changes made through either interface are visible
   *  through the other.
   *
   *  @tparam A the type of the set's elements
   *  @param underlying the wrapped Java set
   */
  @SerialVersionUID(3L)
  class JSetWrapper[A](val underlying: ju.Set[A])
    extends mutable.AbstractSet[A]
      with mutable.SetOps[A, mutable.Set, mutable.Set[A]]
      with StrictOptimizedSetOps[A, mutable.Set, mutable.Set[A]]
      with Serializable {

    override def size: Int = underlying.size
    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    def iterator: Iterator[A] = underlying.iterator.asScala

    def contains(elem: A): Boolean = underlying.contains(elem)

    def addOne(elem: A): this.type = { underlying.add(elem); this }
    def subtractOne(elem: A): this.type = { underlying.remove(elem); this }

    override def remove(elem: A): Boolean = underlying.remove(elem)

    override def clear(): Unit = {
      underlying.clear()
    }

    override def empty: mutable.Set[A] = new JSetWrapper(new ju.HashSet[A])

    // Note: Clone cannot just call underlying.clone because in Java, only specific collections
    // expose clone methods.  Generically, they're protected.
    override def clone(): mutable.Set[A] = new JSetWrapper[A](new ju.LinkedHashSet[A](underlying))

    override def iterableFactory: IterableFactory[mutable.Set] = mutable.HashSet

    override def filterInPlace(p: A => Boolean): this.type = {
      if (underlying.size() > 0) underlying.removeIf(!p(_))
      this
    }
  }

  /** Wraps a Scala `Map` as a Java `Map`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped map at
   *  all times. Modification through the Java interface is supported only
   *  when the wrapped map is mutable: `put`, `remove` and `clear` are added
   *  by [[MutableMapWrapper]], and the entry-set view's `remove` and
   *  `setValue` operations work only on a mutable wrapped map.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @param underlying the wrapped Scala map
   */
  @SerialVersionUID(3L)
  class MapWrapper[K, V](underlying: Map[K, V]) extends ju.AbstractMap[K, V] with Serializable {
    self: MapWrapper[K, V] =>
    override def size = underlying.size

    override def get(key: AnyRef): V = try {
      underlying get key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    override def entrySet: ju.Set[ju.Map.Entry[K, V]] = new ju.AbstractSet[ju.Map.Entry[K, V]] {
      def size = self.size

      def iterator: ju.Iterator[ju.Map.Entry[K, V]] = new ju.Iterator[ju.Map.Entry[K, V]] {
        val ui = underlying.iterator
        var prev : Option[K] = None

        def hasNext = ui.hasNext

        def next(): ju.Map.Entry[K, V] = {
          val (k, v) = ui.next()
          prev = Some(k)
          new ju.Map.Entry[K, V] {
            def getKey = k
            def getValue = v
            def setValue(v1 : V): V = self.put(k, v1)

            // It's important that this implementation conform to the contract
            // specified in the javadocs of java.util.Map.Entry.hashCode
            //
            // See https://github.com/scala/bug/issues/10663
            override def hashCode() =
              java.util.Objects.hashCode(k) ^ java.util.Objects.hashCode(v)

            override def equals(other: Any) = other match {
              case e: ju.Map.Entry[?, ?] => k == e.getKey && v == e.getValue
              case _ => false
            }
          }
        }

        override def remove(): Unit = {
          prev match {
            case Some(k) =>
              underlying match {
                case mm: mutable.Map[a, ?] =>
                  mm -= k
                  prev = None
                case _ =>
                  throw new UnsupportedOperationException("remove")
              }
            case _ =>
              throw new IllegalStateException("next must be called at least once before remove")
          }
        }
      }
    }

    override def containsKey(key: AnyRef): Boolean = try {
      // Note: Subclass of collection.Map with specific key type may redirect generic
      // contains to specific contains, which will throw a ClassCastException if the
      // wrong type is passed. This is why we need a type cast to A inside a try/catch.
      underlying.contains(key.asInstanceOf[K])
    } catch {
      case ex: ClassCastException => false
    }
  }

  /** Wraps a Scala `mutable.Map` as a Java `Map`.
   *
   *  Extends [[MapWrapper]] with the mutating operations `put`, `remove` and
   *  `clear`; changes made through either interface are visible through the
   *  other.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @param underlying the wrapped Scala map
   */
  @SerialVersionUID(3L)
  class MutableMapWrapper[K, V](val underlying: mutable.Map[K, V]) extends MapWrapper[K, V](underlying) {
    override def put(k: K, v: V): V = underlying.put(k, v) match {
      case Some(v1) => v1
      case None => null.asInstanceOf[V]
    }

    override def remove(k: AnyRef): V = try {
      underlying remove k.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    override def clear() = underlying.clear()
  }

  /** Abstract superclass of the wrappers that expose a Java `Map` as a Scala
   *  `mutable.Map`; the shared implementation lives in [[JMapWrapperLike]].
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   */
  @SerialVersionUID(3L)
  abstract class AbstractJMapWrapper[K, V]
    extends mutable.AbstractMap[K, V]
      with JMapWrapperLike[K, V, mutable.Map, mutable.Map[K, V]] with Serializable

  /** Implements a Scala `mutable.Map` in terms of an underlying Java `Map`.
   *
   *  Operations delegate to the wrapped map, so the result is a view: changes
   *  made through either interface are visible through the other. Where the
   *  wrapped map permits `null` values, a key bound to `null` is reported as
   *  `Some(null)` by `get`, `put` and `remove`.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @tparam CC the type constructor of the map returned by transformation operations
   *  @tparam C the type of the map returned by operations that preserve the key and value types
   */
  trait JMapWrapperLike[K, V, +CC[X, Y] <: mutable.MapOps[X, Y, CC, ?], +C <: mutable.MapOps[K, V, CC, C]]
    extends mutable.MapOps[K, V, CC, C]
      with StrictOptimizedMapOps[K, V, CC, C]
      with StrictOptimizedIterableOps[(K, V), mutable.Iterable, C] {

    def underlying: ju.Map[K, V]

    override def size = underlying.size

    // support Some(null) if currently bound to null
    def get(k: K) = {
      val v = underlying.get(k)
      if (v != null)
        Some(v)
      else if (underlying.containsKey(k))
        Some(null.asInstanceOf[V])
      else
        None
    }

    override def getOrElseUpdate(key: K, op: => V): V =
      underlying.computeIfAbsent(key, _ => op) match {
        case null => update(key, null.asInstanceOf[V]); null.asInstanceOf[V]
        case v    => v
      }

    def addOne(kv: (K, V)): this.type = { underlying.put(kv._1, kv._2); this }
    def subtractOne(key: K): this.type = { underlying.remove(key); this }

    // support Some(null) if currently bound to null
    override def put(k: K, v: V): Option[V] =
      if (v == null) {
        val present = underlying.containsKey(k)
        val result  = underlying.put(k, v)
        if (present) Some(result) else None
      } else {
        var result: Option[V] = None
        def recompute(k0: K, v0: V): V = v.tap(_ =>
          if (v0 != null) result = Some(v0)
          else if (underlying.containsKey(k0)) result = Some(null.asInstanceOf[V])
        )
        underlying.compute(k, recompute)
        result
      }

    override def update(k: K, v: V): Unit = underlying.put(k, v)

    override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
      def remap(k: K, v: V): V =
        remappingFunction(Option(v)) match {
          case Some(null) => throw PutNull
          case Some(x)    => x
          case None       => null.asInstanceOf[V]
        }
      try Option(underlying.compute(key, remap))
      catch {
        case PutNull => update(key, null.asInstanceOf[V]); Some(null.asInstanceOf[V])
      }
    }

    // support Some(null) if currently bound to null
    override def remove(k: K): Option[V] = {
      var result: Option[V] = None
      def recompute(k0: K, v0: V): V = {
        if (v0 != null) result = Some(v0)
        else if (underlying.containsKey(k0)) result = Some(null.asInstanceOf[V])
        null.asInstanceOf[V]
      }
      underlying.compute(k, recompute)
      result
    }

    def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
      val ui: java.util.Iterator[java.util.Map.Entry[K, V]] = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next() = { val e = ui.next(); (e.getKey, e.getValue) }
    }

    override def foreachEntry[U](f: (K, V) => U): Unit = {
      val i = underlying.entrySet().iterator()
      while (i.hasNext) {
        val entry = i.next()
        f(entry.getKey, entry.getValue)
      }
    }

    override def clear() = underlying.clear()

  }

  /** Wraps a Java map as a Scala one.  If the map is to support concurrent access,
    * use [[JConcurrentMapWrapper]] instead.  If the wrapped map is synchronized
    * (e.g. from `java.util.Collections.synchronizedMap`), it is your responsibility
    * to wrap all non-atomic operations with `underlying.synchronized`.
    * This includes `get`, as `java.util.Map`'s API does not allow for an
    * atomic `get` when `null` values may be present.
    */
  @SerialVersionUID(3L)
  class JMapWrapper[K, V](val underlying : ju.Map[K, V])
    extends AbstractJMapWrapper[K, V] with Serializable {

    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    override def empty: JMapWrapper[K, V] = new JMapWrapper(new ju.HashMap[K, V])
  }

  /** Wraps a Scala `concurrent.Map` as a Java `java.util.concurrent.ConcurrentMap`.
   *
   *  The wrapper is a view: changes made through either interface are visible
   *  through the other. The single-entry operations below delegate to the
   *  wrapped map's atomic operations of the same name.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @param underlying the wrapped Scala concurrent map
   */
  @SerialVersionUID(3L)
  class ConcurrentMapWrapper[K, V](underlying: concurrent.Map[K, V]) extends MutableMapWrapper[K, V](underlying) with juc.ConcurrentMap[K, V] {

    def underlyingConcurrentMap: concurrent.Map[K, V] = underlying

    override def putIfAbsent(k: K, v: V): V = underlying.putIfAbsent(k, v).getOrElse(null.asInstanceOf[V])

    override def remove(k: AnyRef, v: AnyRef) =
      try underlying.remove(k.asInstanceOf[K], v.asInstanceOf[V])
      catch { case ex: ClassCastException => false }

    override def replace(k: K, v: V): V = underlying.replace(k, v).getOrElse(null.asInstanceOf[V])

    override def replace(k: K, oldval: V, newval: V) = underlying.replace(k, oldval, newval)
  }

  /** Wraps a concurrent Java map as a Scala one.  Single-element concurrent
   *  access is supported; multi-element operations such as maps and filters
   *  are not guaranteed to be atomic.
   */
  @SerialVersionUID(3L)
  class JConcurrentMapWrapper[K, V](val underlying: juc.ConcurrentMap[K, V])
    extends AbstractJMapWrapper[K, V]
      with concurrent.Map[K, V] {

    override def get(k: K) = Option(underlying.get(k))

    /** Returns the value bound to `key`; if the key is absent, evaluates
     *  `op`, stores its result, and returns it.
     *
     *  Delegates to the wrapped map's `computeIfAbsent`. Whether that is atomic depends on
     *  the wrapped map: `ConcurrentHashMap` performs it atomically, but `ConcurrentMap`'s
     *  default implementation does not, and an arbitrary implementation may use it. If `op`
     *  returns `null`, this falls back to a non-atomic check-then-act.
     *
     *  @param key the key to look up
     *  @param op the value to compute if `key` is absent; may be evaluated a
     *            second time if it returns `null`
     */
    override def getOrElseUpdate(key: K, op: => V): V =
      underlying.computeIfAbsent(key, _ => op) match {
        case null => super/*[concurrent.Map]*/.getOrElseUpdate(key, op)
        case v    => v
      }

    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    override def empty: JConcurrentMapWrapper[K, V] = new JConcurrentMapWrapper(new juc.ConcurrentHashMap[K, V])

    def putIfAbsent(k: K, v: V): Option[V] = Option(underlying.putIfAbsent(k, v))

    def remove(k: K, v: V): Boolean = underlying.remove(k, v)

    def replace(k: K, v: V): Option[V] = Option(underlying.replace(k, v))

    def replace(k: K, oldvalue: V, newvalue: V): Boolean = underlying.replace(k, oldvalue, newvalue)

    override def lastOption: Option[(K, V)] =
      underlying match {
        case nav: NavigableMap[K @unchecked, V @unchecked] => Option(nav.lastEntry).map(e => (e.getKey, e.getValue))
        case _ if isEmpty => None
        case _ => Try(last).toOption
      }

    /** Updates the binding for `key` using `remappingFunction` applied to
     *  the currently bound value, `None` if the key is absent.
     *
     *  A result of `Some(v)` stores `v`; `None` removes the binding.
     *  Delegates to the wrapped map's `compute`, which is atomic per the
     *  wrapped map. Whether that is atomic depends on the implementation: `ConcurrentHashMap`
     *  performs it atomically, but `ConcurrentMap`'s default implementation does not. A
     *  `Some(null)` result falls back to a non-atomic implementation in any case.
     *
     *  @param key the key whose binding to update
     *  @param remappingFunction the function computing the new binding from the current one
     *  @return the value now bound to `key`, or `None` if the binding was
     *          removed or the key was absent
     */
    override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
      def remap(k: K, v: V): V =
        remappingFunction(Option(v)) match {
          case Some(null) => throw PutNull // see scala/scala#10129
          case Some(x)    => x
          case None       => null.asInstanceOf[V]
        }
      try Option(underlying.compute(key, remap))
      catch {
        case PutNull => super/*[concurrent.Map]*/.updateWith(key)(remappingFunction)
      }
    }
  }

  /** Wraps a Scala `mutable.Map` as a Java `Dictionary`.
   *
   *  The wrapper is a view: changes made through either interface are
   *  visible through the other.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @param underlying the wrapped Scala map
   */
  @SerialVersionUID(3L)
  class DictionaryWrapper[K, V](val underlying: mutable.Map[K, V]) extends ju.Dictionary[K, V] with Serializable {
    def size: Int = underlying.size
    def isEmpty: Boolean = underlying.isEmpty
    def keys: ju.Enumeration[K] = underlying.keysIterator.asJavaEnumeration
    def elements: ju.Enumeration[V] = underlying.valuesIterator.asJavaEnumeration
    def get(key: AnyRef): V = try {
      underlying get key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }
    def put(key: K, value: V): V = underlying.put(key, value) match {
      case Some(v) => v
      case None => null.asInstanceOf[V]
    }
    override def remove(key: AnyRef): V = try {
      underlying remove key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    override def equals(other: Any): Boolean = other match {
      case that: DictionaryWrapper[?, ?] => this.underlying == that.underlying
      case _ => false
    }

    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Dictionary` as a Scala `mutable.Map`.
   *
   *  The wrapper is a view: changes made through either interface are
   *  visible through the other. (`Dictionary` permits neither `null` keys
   *  nor `null` values.)
   *
   *  @tparam K the type of the dictionary's keys
   *  @tparam V the type of the dictionary's values
   *  @param underlying the wrapped Java dictionary
   */
  @SerialVersionUID(3L)
  class JDictionaryWrapper[K, V](val underlying: ju.Dictionary[K, V]) extends mutable.AbstractMap[K, V] with Serializable {
    override def size: Int = underlying.size
    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize

    def get(k: K) = Option(underlying.get(k))

    def addOne(kv: (K, V)): this.type = { underlying.put(kv._1, kv._2); this }
    def subtractOne(key: K): this.type = { underlying.remove(key); this }

    override def put(k: K, v: V): Option[V] = Option(underlying.put(k, v))

    override def update(k: K, v: V): Unit = { underlying.put(k, v) }

    override def remove(k: K): Option[V] = Option(underlying.remove(k))
    def iterator = underlying.keys.asScala map (k => (k, underlying.get(k)))

    override def clear() = iterator.foreach(entry => underlying.remove(entry._1))

    override def mapFactory: mutable.HashMap.type = mutable.HashMap
  }

  /** Wraps a `java.util.Properties` object as a Scala
   *  `mutable.Map[String, String]`.
   *
   *  The wrapper is a view: changes made through either interface are
   *  visible through the other. The `Map` operations access only the
   *  properties object's own entries, which are assumed to be strings; any
   *  default properties are consulted only by `getProperty`.
   *
   *  @param underlying the wrapped properties object
   */
  @SerialVersionUID(3L)
  class JPropertiesWrapper(underlying: ju.Properties)
    extends mutable.AbstractMap[String, String]
      with mutable.MapOps[String, String, mutable.Map, mutable.Map[String, String]]
      with StrictOptimizedMapOps[String, String, mutable.Map, mutable.Map[String, String]]
      with StrictOptimizedIterableOps[(String, String), mutable.Iterable, mutable.Map[String, String]]
      with Serializable {

    override def size = underlying.size
    override def isEmpty: Boolean = underlying.isEmpty
    override def knownSize: Int = size
    def get(k: String) = {
      val v = underlying.get(k)
      if (v != null) Some(v.asInstanceOf[String]) else None
    }

    def addOne(kv: (String, String)): this.type = { underlying.put(kv._1, kv._2); this }
    def subtractOne(key: String): this.type = { underlying.remove(key); this }

    override def put(k: String, v: String): Option[String] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    override def update(k: String, v: String): Unit = { underlying.put(k, v) }

    override def remove(k: String): Option[String] = {
      val r = underlying.remove(k)
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    def iterator: Iterator[(String, String)] = new AbstractIterator[(String, String)] {
      val ui: java.util.Iterator[java.util.Map.Entry[Object, Object]] = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next() = {
        val e = ui.next()
        (e.getKey.asInstanceOf[String], e.getValue.asInstanceOf[String])
      }
    }

    override def clear() = underlying.clear()

    override def empty: JPropertiesWrapper = new JPropertiesWrapper(new ju.Properties)

    def getProperty(key: String): String | Null = underlying.getProperty(key)

    def getProperty(key: String, defaultValue: String): String = underlying.getProperty(key, defaultValue)

    def setProperty(key: String, value: String): AnyRef | Null =
      underlying.setProperty(key, value)

    override def mapFactory: mutable.HashMap.type = mutable.HashMap
  }

  /** Thrown when certain Map operations attempt to put a null value. */
  private val PutNull = new ControlThrowable {}
}
