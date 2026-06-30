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

import scala.language.`2.13`

/** This class provides a simple way to get unique objects for equal strings.
 *  Since symbols are interned, they can be compared using reference equality.
 */
final class Symbol private (val name: String) extends Serializable {
  /** A string representation of this symbol. */
  override def toString(): String = s"Symbol($name)"

  @throws(classOf[java.io.ObjectStreamException])
  private def readResolve(): Any = Symbol.apply(name)
  /** Returns the hash code of this symbol, derived from its name. */
  override def hashCode() = name.hashCode()
  /** Tests whether `other` is this same symbol. Because symbols are interned,
   *  this reduces to reference equality.
   *
   *  @param other the object to compare with this symbol
   */
  override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
}

object Symbol extends UniquenessCache[String, Symbol] {
  /** Returns the unique symbol for `name`, creating and interning it if it does
   *  not already exist.
   *
   *  @param name the name of the symbol
   */
  override def apply(name: String): Symbol = super.apply(name)
  /** Creates a fresh symbol with the given name.
   *
   *  @param name the name for the new symbol
   *  @return a newly constructed symbol named `name`
   */
  protected def valueFromKey(name: String): Symbol = new Symbol(name)
  /** Returns the name under which `sym` is cached.
   *
   *  @param sym the symbol whose name to recover
   *  @return the symbol's name, always wrapped in `Some`
   */
  protected def keyFromValue(sym: Symbol): Option[String] = Some(sym.name)
}

/** This is private so it won't appear in the library API, but
 *  abstracted to offer some hope of reusability.  
 *
 *  @tparam K the type of keys used for cache lookup, held via weak references
 *  @tparam V the type of cached values, weakly referenced and constructed from keys via `valueFromKey`
 */
private[scala] abstract class UniquenessCache[K, V] {
  import java.lang.ref.WeakReference
  import java.util.WeakHashMap
  import java.util.concurrent.locks.ReentrantReadWriteLock

  private val rwl = new ReentrantReadWriteLock()
  private val rlock = rwl.readLock
  private val wlock = rwl.writeLock
  private val map = new WeakHashMap[K, WeakReference[V]]

  /** Constructs the value to cache for the given key.
   *
   *  @param k the key to construct a value for
   *  @return the value to associate with `k`
   */
  protected def valueFromKey(k: K): V
  /** Recovers the key under which the given value is cached.
   *
   *  @param v the value whose key to recover
   *  @return the key for `v`, or `None` if it has none
   */
  protected def keyFromValue(v: V): Option[K]

  /** Returns the cached value for `name`, constructing and caching it on first use.
   *
   *  @param name the key to look up
   */
  def apply(name: K): V = {
    /** Returns the currently cached value for `name`, or `null` if none is cached
     *  or it has been garbage-collected.
     */
    def cached(): V | Null = {
      rlock.lock
      try {
        val reference = map.get(name)
        if (reference == null) null
        else reference.get  // will be null if we were gc-ed
      }
      finally rlock.unlock
    }
    /** Returns the cached value for `name`, computing and storing it under the
     *  write lock when it is not already present.
     */
    def updateCache(): V = {
      wlock.lock
      try {
        val res = cached()
        if (res != null) res
        else {
          // If we don't remove the old String key from the map, we can
          // wind up with one String as the key and a different String as
          // the name field in the Symbol, which can lead to surprising GC
          // behavior and duplicate Symbols. See scala/bug#6706.
          map.remove(name)
          val sym = valueFromKey(name)
          map.put(name, new WeakReference(sym))
          sym
        }
      }
      finally wlock.unlock
    }
    cached() match {
      case null => updateCache()
      case res  => res
    }
  }
  /** Returns the key corresponding to `other`, enabling use as an extractor in
   *  pattern matches.
   *
   *  @param other the value to extract a key from
   *  @return the key for `other`, or `None` if it has none
   */
  def unapply(other: V): Option[K] = keyFromValue(other)
}
