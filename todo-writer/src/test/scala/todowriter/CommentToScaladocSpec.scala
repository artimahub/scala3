package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

class CommentToScaladocSpec extends AnyFlatSpec with Matchers:

  private def withTempFile(content: String)(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("todowriter-test")
    val tempFile = tempDir.resolve("Test.scala")
    try
      Files.writeString(tempFile, content)
      test(tempFile)
    finally
      Files.deleteIfExists(tempFile)
      Files.deleteIfExists(tempDir)

  "CommentToScaladoc" should "convert block comment before class to scaladoc" in {
    val content = """package test

/* This is a class comment */
class MyClass:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

/** This is a class comment */
class MyClass:
  pass
"""))
    }
  }

  it should "convert block comment before trait to scaladoc" in {
    val content = """package test

/* This is a trait comment */
trait MyTrait:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

/** This is a trait comment */
trait MyTrait:
  pass
"""))
    }
  }

  it should "convert block comment before type alias to scaladoc" in {
    val content = """package test

/* Type alias for Int */
type MyInt = Int
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

/** Type alias for Int */
type MyInt = Int
"""))
    }
  }

  it should "convert block comment before def to scaladoc" in {
    val content = """package test

class MyClass:
  /* This is a method */
  def foo: Int = 42
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

class MyClass:
  /** This is a method */
  def foo: Int = 42
"""))
    }
  }

  it should "convert block comment before val to scaladoc" in {
    val content = """package test

class MyClass:
  /* This is a value */
  val myVal: Int = 42
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

class MyClass:
  /** This is a value */
  val myVal: Int = 42
"""))
    }
  }

  it should "convert block comment before var to scaladoc" in {
    val content = """package test

class MyClass:
  /* This is a variable */
  var myVar: Int = 42
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

class MyClass:
  /** This is a variable */
  var myVar: Int = 42
"""))
    }
  }

  it should "preserve multi-line block comments" in {
    val content = """package test

/* This is a multi-line
   comment about the class
   with several lines */
class MyClass:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

/** This is a multi-line
   comment about the class
   with several lines */
class MyClass:
  pass
"""))
    }
  }

  it should "not convert comments that are not before declarations" in {
    val content = """package test

/* This is just a comment */
val x = 42

class MyClass:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(None)  // No changes
    }
  }

  it should "not convert existing scaladoc comments" in {
    val content = """package test

/** Already scaladoc */
class MyClass:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(None)  // No changes
    }
  }

  it should "handle multiple conversions in same file" in {
    val content = """package test

/* First comment */
class First:
  pass

/* Second comment */
class Second:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

/** First comment */
class First:
  pass

/** Second comment */
class Second:
  pass
"""))
    }
  }

  it should "not convert line comments" in {
    val content = """package test

// This is a line comment
class MyClass:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(None)  // No changes
    }
  }

  it should "handle whitespace before comment" in {
    val content = """package test

  /* Indented comment */
  class MyClass:
    pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(Some("""package test

  /** Indented comment */
  class MyClass:
    pass
"""))
    }
  }

  it should "not convert a license comment at the top of the file (before package)" in {
    val content = """/* Copyright 2026 EPFL and contributors.
 * Licensed under Apache License 2.0.
 */

package test

class MyClass:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(None)  // No changes – license comment must not be touched
    }
  }

  it should "not convert any block comment that appears before the first package declaration" in {
    val content = """/* Preamble: this file is part of the project. */

package test

/* This is a class comment */
class MyClass:
  pass
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      // Only the second comment (after 'package') should be converted
      result should be(Some("""/* Preamble: this file is part of the project. */

package test

/** This is a class comment */
class MyClass:
  pass
"""))
    }
  }

  it should "not convert a commented-out code block before a declaration" in {
    // The block contains Scala code lines (def, val, etc.) without ' * ' prefixes,
    // which makes it a commented-out code block, not a documentation comment.
    val content = """package test

  /*
  private final def checkInvariantSubNodesAreCompacted(): Boolean =
    new SomeIterator[K, V](this).size >= 2 * nodeArity

  private final def checkInvariantContentIsWellTyped(): Boolean = {
    val predicate1 = TupleLength * payloadArity + nodeArity == content.length

    val predicate3 = Range(0, content.length)
      .forall(i => content(i).isInstanceOf[MapNode[_, _]] == true)

    predicate1
  }
  */
  def getKey(index: Int): K = ???
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(None)  // Must not touch a commented-out code block
    }
  }

  it should "not convert an inline comment on the same line as code before it" in {
    // /*@volatile*/ appears on the same line as the var declaration it annotates,
    // followed on the next line by more code (which makes hasIndentation=true)
    val content = """package test

class MyClass:
  /*@volatile*/ @annotation.stableNull private var _result: Int = 0
  def result: Int = _result
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(None)  // inline comment must not be touched
    }
  }

  it should "not convert an inline comment that has code after it on the same line" in {
    // /* if(...) */ false — code follows the comment on the same line
    val content = """package test

class MyClass:
  def foo: Boolean = /* if(someCondition) */ false
""".stripMargin

    withTempFile(content) { path =>
      val result = CommentToScaladoc.convertFile(path)
      result should be(None)  // inline comment must not be touched
    }
  }
