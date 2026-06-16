package todowriter

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Path, Paths}
import java.io.PrintWriter

class LinkCheckerSpec extends AnyFunSuite {

  test("MutationTracker.checkMutations() and MutationTracker.checkMutationsForIteration() should both be valid") {
    // Create a temporary directory with test files
    val tempDir = Files.createTempDirectory("link-checker-test")
    try {
      // Create a test Scala file with the MutationTracker object
      val mutationTrackerFile = tempDir.resolve("MutationTracker.scala")
      val writer = new PrintWriter(mutationTrackerFile.toFile)
      writer.println("""
package scala.collection.mutable

private object MutationTracker {

  def checkMutations(expectedCount: Int, actualCount: Int, message: String): Unit = {
    if (actualCount != expectedCount) throw new RuntimeException(message)
  }

  def checkMutationsForIteration(expectedCount: Int, actualCount: Int): Unit =
    checkMutations(expectedCount, actualCount, "mutation occurred during iteration")
}
      """.trim)
      writer.close()

      // Create a test Scala file with the scaladoc
      val testFile = tempDir.resolve("Test.scala")
      val writer2 = new PrintWriter(testFile.toFile)
      writer2.println("""
package test

/**
 * Test scaladoc with both method references.
 * [[MutationTracker.checkMutations() `checkMutations`]]
 * and [[MutationTracker.checkMutationsForIteration() `checkMutationsForIteration`]]
 */
class Test
      """.trim)
      writer2.close()

      // Run the link checker
      val brokenLinks = ScaladocChecker.findBrokenLinks(tempDir)

      // Print the results for debugging
      println(s"Broken links found: ${brokenLinks.size}")
      for (path, line, url, err) <- brokenLinks do
        println(s"  $path:$line -> $url => $err")

      // Both should be valid (no broken links)
      assert(brokenLinks.isEmpty, s"Expected no broken links, but found: ${brokenLinks.mkString(", ")}")
    } finally {
      // Clean up - delete files in reverse order to handle nested directories
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(f => Files.deleteIfExists(f))
    }
  }

  test("MutationTracker.checkMutations (without parens) should be valid") {
    // Create a temporary directory with test files
    val tempDir = Files.createTempDirectory("link-checker-test2")
    try {
      // Create a test Scala file with the MutationTracker object
      val mutationTrackerFile = tempDir.resolve("MutationTracker.scala")
      val writer = new PrintWriter(mutationTrackerFile.toFile)
      writer.println("""package scala.collection.mutable

private object MutationTracker {

  def checkMutations(expectedCount: Int, actualCount: Int, message: String): Unit = {
    if (actualCount != expectedCount) throw new RuntimeException(message)
  }

  def checkMutationsForIteration(expectedCount: Int, actualCount: Int): Unit =
    checkMutations(expectedCount, actualCount, "mutation occurred during iteration")
}
      """.trim)
      writer.close()

      // Create a test Scala file with the scaladoc
      val testFile = tempDir.resolve("Test.scala")
      val writer2 = new PrintWriter(testFile.toFile)
      writer2.println("""package test

/**
 * Test scaladoc with method reference without parens.
 * [[MutationTracker.checkMutations `checkMutations`]]
 */
class Test
      """.trim)
      writer2.close()

      // Run the link checker
      val brokenLinks = ScaladocChecker.findBrokenLinks(tempDir)

      // Print the results for debugging
      println(s"Broken links found: ${brokenLinks.size}")
      for (path, line, url, err) <- brokenLinks do
        println(s"  $path:$line -> $url => $err")

      // Should be valid (no broken links)
      assert(brokenLinks.isEmpty, s"Expected no broken links, but found: ${brokenLinks.mkString(", ")}")
    } finally {
      // Clean up - delete files in reverse order to handle nested directories
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(f => Files.deleteIfExists(f))
    }
  }

  test("Method reference with empty parens should be valid when method has no parameters") {
    // Create a temporary directory with test files
    val tempDir = Files.createTempDirectory("link-checker-test3")
    try {
      // Create a test Scala file with an object that has a no-arg method
      val objFile = tempDir.resolve("MyObject.scala")
      val writer = new PrintWriter(objFile.toFile)
      writer.println("""package test

object MyObject {
  def noParamMethod(): String = "hello"
  def methodWithParam(x: Int): String = x.toString
}
      """.trim)
      writer.close()

      // Create a test Scala file with the scaladoc
      val testFile = tempDir.resolve("Test.scala")
      val writer2 = new PrintWriter(testFile.toFile)
      writer2.println("""package test

/**
 * Test scaladoc with empty parens.
 * [[MyObject.noParamMethod() `noParamMethod`]]
 * [[MyObject.methodWithParam() `methodWithParam`]]
 */
class Test
      """.trim)
      writer2.close()

      // Run the link checker
      val brokenLinks = ScaladocChecker.findBrokenLinks(tempDir)

      // Print the results for debugging
      println(s"Broken links found: ${brokenLinks.size}")
      for (path, line, url, err) <- brokenLinks do
        println(s"  $path:$line -> $url => $err")

      // Both should be valid (no broken links) - the empty parens should be ignored
      assert(brokenLinks.isEmpty, s"Expected no broken links, but found: ${brokenLinks.mkString(", ")}")
    } finally {
      // Clean up - delete files in reverse order to handle nested directories
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(f => Files.deleteIfExists(f))
    }
  }

  test("MutationTracker self-reference test - scaladoc in same object") {
    // This test mimics the exact scenario from the library where
    // the scaladoc is in the same object that it references
    val tempDir = Files.createTempDirectory("link-checker-test4")
    try {
      // Create the exact structure from MutationTracker.scala
      val mutationTrackerFile = tempDir.resolve("MutationTracker.scala")
      val writer = new PrintWriter(mutationTrackerFile.toFile)
      writer.println("""package scala.collection.mutable

import java.util.ConcurrentModificationException

/**
 * Utilities to check that mutations to a client that tracks
 * its mutations have not occurred since a given point.
 * [[Iterator `Iterator`]]s that perform this check automatically
 * during iteration can be created by wrapping an `Iterator`
 * in a [[MutationTracker.CheckedIterator `CheckedIterator`]],
 * or by manually using the [[MutationTracker.checkMutations() `checkMutations`]]
 * and [[MutationTracker.checkMutationsForIteration() `checkMutationsForIteration`]]
 * methods.
 */
private object MutationTracker {

  /**
   * Checks whether or not the actual mutation count differs from
   * the expected one, throwing an exception, if it does.
   *
   * @param expectedCount the expected mutation count
   * @param actualCount   the actual mutation count
   * @param message the exception message in case of mutations
   * @throws ConcurrentModificationException if the expected and actual
   *                                         mutation counts differ
   */
  @throws[ConcurrentModificationException]
  def checkMutations(expectedCount: Int, actualCount: Int, message: String): Unit = {
    if (actualCount != expectedCount) throw new ConcurrentModificationException(message)
  }

  /**
   * Checks whether or not the actual mutation count differs from
   * the expected one, throwing an exception, if it does. This method
   * produces an exception message saying that it was called because a
   * backing collection was mutated during iteration.
   *
   * @param expectedCount the expected mutation count
   * @param actualCount   the actual mutation count
   * @throws ConcurrentModificationException if the expected and actual
   *                                         mutation counts differ
   */
  @throws[ConcurrentModificationException]
  @inline def checkMutationsForIteration(expectedCount: Int, actualCount: Int): Unit =
    checkMutations(expectedCount, actualCount, "mutation occurred during iteration")

  /**
   * An iterator wrapper that checks if the underlying collection has
   * been mutated.
   *
   * @param underlying    the underlying iterator
   * @param mutationCount a by-name provider of the current mutation count
   * @tparam A the type of the iterator's elements
   */
  final class CheckedIterator[A](underlying: Iterator[A], mutationCount: => Int) extends AbstractIterator[A] {
    private val expectedCount = mutationCount

    def hasNext: Boolean = {
      checkMutationsForIteration(expectedCount, mutationCount)
      underlying.hasNext
    }
    def next(): A = underlying.next()
  }
}
      """.trim)
      writer.close()

      // Run the link checker
      val brokenLinks = ScaladocChecker.findBrokenLinks(tempDir)

      // Print the results for debugging
      println(s"\nBroken links found: ${brokenLinks.size}")
      for (path, line, url, err) <- brokenLinks do
        println(s"  $path:$line -> $url => $err")

      // Both should be valid (no broken links)
      assert(brokenLinks.isEmpty, s"Expected no broken links, but found: ${brokenLinks.mkString(", ")}")
    } finally {
      // Clean up - delete files in reverse order to handle nested directories
      Files.walk(tempDir)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(f => Files.deleteIfExists(f))
    }
  }
}