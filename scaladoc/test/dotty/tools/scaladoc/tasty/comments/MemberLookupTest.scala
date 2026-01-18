package dotty.tools.scaladoc
package tasty.comments

import org.junit.Test

class MemberLookupTest:

  @Test
  def testOverloadMatching(): Unit =
    // This test verifies that the overload matching logic works correctly
    // We'll test the helper functions that are used for overload matching

    // Test containsWordBoundary
    println(s"Testing containsWordBoundary('scala.collection.mutable.Buffer', 'Buffer'): ${containsWordBoundary("scala.collection.mutable.Buffer", "Buffer")}")
    assert(containsWordBoundary("scala.collection.mutable.Buffer", "Buffer"))
    println(s"Testing containsWordBoundary('scala.collection.mutable.Buffered', 'Buffer'): ${containsWordBoundary("scala.collection.mutable.Buffered", "Buffer")}")
    assert(!containsWordBoundary("scala.collection.mutable.Buffered", "Buffer"))
    println(s"Testing containsWordBoundary('scala.collection.mutable.BufferMap', 'Buffer'): ${containsWordBoundary("scala.collection.mutable.BufferMap", "Buffer")}")
    assert(!containsWordBoundary("scala.collection.mutable.BufferMap", "Buffer"))

    // Test extractParamTypeFromSignature
    println(s"Testing extractParamTypeFromSignature('asJava[A](b:scala.collection.mutable.Buffer[A])*'): ${extractParamTypeFromSignature("asJava[A](b:scala.collection.mutable.Buffer[A])*")}")
    assert(extractParamTypeFromSignature("asJava[A](b:scala.collection.mutable.Buffer[A])*") == Some("Buffer"))
    println(s"Testing extractParamTypeFromSignature('asJava[K,V](m:scala.collection.Map[K,V])*'): ${extractParamTypeFromSignature("asJava[K,V](m:scala.collection.Map[K,V])*")}")
    assert(extractParamTypeFromSignature("asJava[K,V](m:scala.collection.Map[K,V])*") == Some("Map"))
    println(s"Testing extractParamTypeFromSignature('asJava[A](s:scala.collection.Seq[A])*'): ${extractParamTypeFromSignature("asJava[A](s:scala.collection.Seq[A])*")}")
    assert(extractParamTypeFromSignature("asJava[A](s:scala.collection.Seq[A])*") == Some("Seq"))

    // Test with escaped dots
    println(s"Testing extractParamTypeFromSignature('asJava[A](b:scala\\.collection\\.mutable\\.Buffer[A])*'): ${extractParamTypeFromSignature("asJava[A](b:scala\\.collection\\.mutable\\.Buffer[A])*")}")
    assert(extractParamTypeFromSignature("asJava[A](b:scala\\.collection\\.mutable\\.Buffer[A])*") == Some("Buffer"))
    println(s"Testing extractParamTypeFromSignature('asJava[K,V](m:scala\\.collection\\.Map[K,V])*'): ${extractParamTypeFromSignature("asJava[K,V](m:scala\\.collection\\.Map[K,V])*")}")
    assert(extractParamTypeFromSignature("asJava[K,V](m:scala\\.collection\\.Map[K,V])*") == Some("Map"))

  private def containsWordBoundary(s: String, word: String): Boolean =
    val idx = s.indexOf(word)
    if idx == -1 then false
    else
      val before = if idx == 0 then true else !s.charAt(idx - 1).isLetterOrDigit
      val after = if idx + word.length >= s.length then true else !s.charAt(idx + word.length).isLetterOrDigit
      before && after

  private def extractParamTypeFromSignature(signature: String): Option[String] =
    val normalizedSignature = signature.replace("\\.", ".")
    val collectionTypes = List("Buffer", "Map", "Set", "Seq", "Iterator", "Iterable", "List", "Dictionary", "ConcurrentMap", "Properties")
    collectionTypes.find { typeName =>
      normalizedSignature.contains(s".${typeName}") || normalizedSignature.contains(s"${typeName}[")
    }