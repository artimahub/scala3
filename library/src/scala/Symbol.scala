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
  /** Returns the hash code for this symbol, which is the hash code of its name. */
  override def hashCode() = name.hashCode()
  /** Tests whether `other` is this same symbol, using reference equality.
   *
   *  Because symbols are interned, two symbols are equal exactly when they are
   *  the same object, so reference equality suffices.
   *
   *  @param other the object to compare with this symbol
   */
  override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
}

object Symbol extends UniquenessCache[String, Symbol] {
  /** Returns the unique, interned symbol with the given name. Repeated calls
   *  with equal names return the same object, so results can be compared with
   *  reference equality.
   *
   *  @param name the name of the symbol
   */
  override def apply(name: String): Symbol = super.apply(name)
  /** Creates a new symbol wrapping the given name.
   *
   *  @param name the name to wrap
   *  @return a freshly constructed `Symbol` for `name`
   */
  protected def valueFromKey(name: String): Symbol = new Symbol(name)
  /** Extracts the name used as the cache key for the given symbol.
   *
   *  @param sym the symbol whose key is needed
   *  @return the symbol's name, wrapped in `Some`
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

  /** Constructs the value to be cached for the given key.
   *
   *  @param k the key to construct a value from
   *  @return the value corresponding to `k`
   */
  protected def valueFromKey(k: K): V
  /** Extracts the key under which the given value is cached.
   *
   *  @param v the value whose key is needed
   *  @return the key for `v`, or `None` if it has none
   */
  protected def keyFromValue(v: V): Option[K]

  /** Returns the cached value for the given key, constructing and caching it on
   *  first access. Values are held via weak references, so the cached value may
   *  be reconstructed for a key after its previous value has been
   *  garbage-collected. Access is thread-safe.
   *
   *  @param name the key to look up
   */
  def apply(name: K): V = {
    def cached(): V | Null = {
      rlock.lock
      try {
        val reference = map.get(name)
        if (reference == null) null
        else reference.get  // will be null if we were gc-ed
      }
      finally rlock.unlock
    }
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
  /** Extracts the key associated with the given value, enabling use as an
   *  extractor in pattern matches.
   *
   *  @param other the value to extract from
   *  @return the key for `other`, or `None` if it has none
   */
  def unapply(other: V): Option[K] = keyFromValue(other)
}
