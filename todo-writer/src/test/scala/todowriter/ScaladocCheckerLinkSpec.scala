package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import java.nio.file.{Files, Path}
import java.net.ServerSocket
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicBoolean

class ScaladocCheckerLinkSpec extends AnyFlatSpec with Matchers:

  private def withTempFile(content: String)(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("todowriter-link-test")
    val tempFile = tempDir.resolve("LinksTest.scala")
    try
      Files.writeString(tempFile, content)
      test(tempFile)
    finally
      Files.deleteIfExists(tempFile)
      Files.deleteIfExists(tempDir)

  private class SimpleServer(val port: Int, stopFn: () => Unit):
    def getPort: Int = port
    def stop(): Unit = stopFn()

  private def startSimpleServer(handlers: Map[String, Int]): SimpleServer =
    val serverSocket = new ServerSocket(0)
    val port = serverSocket.getLocalPort
    val running = new AtomicBoolean(true)
    val started = new java.util.concurrent.CountDownLatch(1)

    val thread = new Thread(() =>
      try
        // Signal that the server thread has started
        started.countDown()
        while running.get() do
          val socket = serverSocket.accept()
          try
            // Read the request line and consume headers to avoid blocking the client
            socket.setSoTimeout(2000)
            val in = new java.io.BufferedReader(new java.io.InputStreamReader(socket.getInputStream))
            val requestLine =
              try in.readLine()
              catch case _: Throwable => ""
            try
              var line = in.readLine()
              while line != null && line.nonEmpty do line = in.readLine()
            catch case _: Throwable => ()
            try in.close() catch case _: Throwable => ()

            val parts = requestLine.split(" ")
            val path = if parts.length >= 2 then parts(1) else "/"
            val code = handlers.getOrElse(path, 200)
            val out = socket.getOutputStream
            val resp = s"HTTP/1.1 $code OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
            out.write(resp.getBytes("UTF-8"))
            out.flush()
          finally
            try socket.close() catch case _: Throwable => ()
      catch
        case _: Throwable => ()
    )
    thread.setDaemon(true)
    thread.start()
    // Wait for server thread to be running
    started.await()

    SimpleServer(port, () =>
      running.set(false)
      try serverSocket.close() catch case _: Throwable => ()
      try thread.join(100) catch case _: Throwable => ()
    )

  "ScaladocChecker.findBrokenLinks" should "report broken links and ignore valid links" in {
    val server = startSimpleServer(Map("/ok" -> 200, "/notfound" -> 404))
    try
      val port = server.getPort
      val content = s"""package test
                      |
                      |/** Test links.
                      | *  http://localhost:$port/ok
                      | *  http://localhost:$port/notfound
                      | */
                      |def foo(): Unit = ()
                      |""".stripMargin
      withTempFile(content) { path =>
        // Use the injectable checker to avoid timing issues with real network access
        def checker(url: String, decl: todowriter.Declaration): Option[String] =
          if url.endsWith("/ok") then None
          else if url.endsWith("/notfound") then Some("HTTP 404")
          else Some("Error: unknown")
        val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
        val urls = broken.map(_._3)
        urls should contain (s"http://localhost:$port/notfound")
        urls should not contain (s"http://localhost:$port/ok")
        val entry = broken.find(_._3 == s"http://localhost:$port/notfound")
        entry shouldBe defined
        entry.get._4 should include("404")
      }
    finally
      server.stop()
  }

  it should "return no entries when all links are valid" in {
    val server = startSimpleServer(Map("/ok" -> 200))
    try
      val port = server.getPort
      val content = s"""package test
                      |
                      |/** Only good links.
                      | *  http://localhost:$port/ok
                      | */
                      |def foo(): Unit = ()
                      |""".stripMargin
      withTempFile(content) { path =>
        def checker(url: String, decl: todowriter.Declaration): Option[String] =
          if url.endsWith("/ok") then None else Some("Error")
        val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
        broken should be (empty)
      }
    finally
      server.stop()
  }

  it should "report unreachable hosts as errors" in {
    // Use a guaranteed-unresolvable domain under the .invalid TLD
    val badUrl = "http://nonexistent-domain.invalid/path"
    val content = s"""package test
                    |
                    |/** Bad host link.
                    | *  $badUrl
                    | */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      def checker(url: String, decl: todowriter.Declaration): Option[String] =
        if url == badUrl then Some("Error: Unknown host") else None
      val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
      val entryOpt = broken.find(_._3 == badUrl)
      entryOpt shouldBe defined
      entryOpt.get._4.startsWith("Error:") should be (true)
    }
  }

  it should "detect links inside markdown [label](url) and report broken ones" in {
    val content = """package test
                    |
                    |/** Example with markdown link.
                    | *  See [docs](http://example.com/docs)
                    | *  And a bad link: [bad](http://example.com/bad)
                    | */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      def checker(url: String, decl: todowriter.Declaration): Option[String] =
                if url.endsWith("/docs") then None
                else if url.endsWith("/bad") then Some("HTTP 404")
                else Some("Error")
              val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
              val urls = broken.map(_._3)
      urls should contain ("http://example.com/bad")
      urls should not contain ("http://example.com/docs")
      val entry = broken.find(_._3 == "http://example.com/bad")
      entry shouldBe defined
      entry.get._4 should include("404")
    }
  }

  it should "detect code links like [Int](scala.Int)" in {
    val content = """package test
                    |
                    |/** Code link example.
                    | *  Refer to [Int](scala.Int)
                    | *  Refer to [Foo](com.example.Foo)
                    | */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      def checker(url: String, decl: todowriter.Declaration): Option[String] =
                if url == "scala.Int" then None
                else if url == "com.example.Foo" then Some("Symbol not found")
                else Some("Error")
              val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
              val urls = broken.map(_._3)
              urls should contain ("com.example.Foo")
              urls should not contain ("scala.Int")
      val entry = broken.find(_._3 == "com.example.Foo")
      entry shouldBe defined
      entry.get._4 should include("not found")
    }
  }

  it should "symbolExistsInSource finds declarations in nearby source files" in {
    val tempDir = Files.createTempDirectory("symbol-source-test")
    val file1 = tempDir.resolve("Foo.scala")
    val file2 = tempDir.resolve("Bar.scala")
    try
      ScaladocChecker.clearCaches()
      Files.writeString(file1, "package com.example\n\nclass Foo\n")
      Files.writeString(file2, "object Bar\n")
      ScaladocChecker.symbolExistsInSource(tempDir, "com.example.Foo") should be(true)
      ScaladocChecker.symbolExistsInSource(tempDir, "Bar") should be(true)
      ScaladocChecker.symbolExistsInSource(tempDir, "com.example.Missing") should be(false)
      // Also verify case class and trait detection
      val file3 = tempDir.resolve("Baz.scala")
      Files.writeString(file3, "package pkg\n\ncase class Baz(x: Int)\ntrait Qux\n")
      ScaladocChecker.clearCaches()
      ScaladocChecker.symbolExistsInSource(tempDir, "pkg.Baz") should be(true)
      ScaladocChecker.symbolExistsInSource(tempDir, "Qux") should be(true)
    finally
      try Files.deleteIfExists(file1) catch case _: Throwable => ()
      try Files.deleteIfExists(file2) catch case _: Throwable => ()
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "ignore inline code spans (backticks) that contain link-like patterns" in {
    val content = """package test
                    |
                    |/** Creates a method definition `def f[..](...)` with the signature defined in the symbol.
                    | *  Also a real link for control: http://example.com/ok
                    | */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      // Treat any matched URL as broken to detect false positives inside code spans
      def checker(url: String, decl: todowriter.Declaration): Option[String] = Some("HTTP 404")
      val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
      // Only the real http link should be reported (and it's marked broken by checker),
      // the inline code span should not produce any additional entries.
      val urls = broken.map(_._3)
      urls.count(_.startsWith("http://")) should be (1)
      urls.exists(_ == "http://example.com/ok") should be (true)
    }
  }

  it should "ignore fenced code blocks (```...```) containing link-like patterns" in {
    val content = """package test
                    |
                    |/**
                    | * Example:
                    | * ```
                    | * def g[..](...) = ???
                    | * ```
                    | * Also: [good](http://example.com/good)
                    | */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      def checker(url: String, decl: todowriter.Declaration): Option[String] =
                if url.endsWith("/good") then None else Some("HTTP 404")
            val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)      // The code fence should not produce a link; only the markdown link should be considered.
      val urls = broken.map(_._3)
      urls should not contain ("def")
      urls should not contain ("def g[..](...)")
      urls should not contain ("```def g[..](...)```")
      urls should not contain ("[good](http://example.com/good)")
    }
  }

  it should "ignore triple-brace code blocks ({{{ ... }}}) containing link-like patterns" in {
    val content = """package test
                    |
                    |/**
                    | * Example:
                    | * {{{def h[..](...)}}}
                    | * See also http://example.com/x
                    | */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      def checker(url: String, decl: todowriter.Declaration): Option[String] = Some("HTTP 404")
      val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
      val urls = broken.map(_._3)
      // Only the explicit http link should be reported; the triple-brace code should be ignored.
      urls.count(_.startsWith("http://")) should be (1)
    }
  }

  it should "resolve Predef.print method symbol" in {
    val content = """package test
                    |
                    |/** Refer to [print](scala.Predef.print(x:Any)) */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(path.getParent)
      val urls = broken.map(_._3)
      // Expectation: the method symbol should be resolved and NOT be reported as broken.
      urls should not contain ("scala.Predef.print(x:Any)")
    }
  }

  it should "resolve ExecutionContext$.global symbol" in {
    val content = """package test
                    |
                    |/** Refer to [global](scala.concurrent.ExecutionContext$.global) */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(path.getParent)
      val urls = broken.map(_._3)
      // Expectation: the symbol should be resolved and NOT be reported as broken.
      urls should not contain ("scala.concurrent.ExecutionContext$.global")
    }
  }

  it should "resolve object/member symbols like scala.concurrent.duration.Duration.Inf" in {
    val content = """package test
                    |
                    |/** Refer to [Inf](scala.concurrent.duration.Duration.Inf) */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(path.getParent)
      val urls = broken.map(_._3)
      // Expectation: the symbol should be resolved and NOT be reported as broken.
      urls should not contain ("scala.concurrent.duration.Duration.Inf")
    }
  }

  it should "handle unqualified/malformed method references like Predef.print(x:Any)" in {
    val content = """package test
                    |
                    |/** Refer to [print](Predef.print(x:Any) */
                    |def foo(): Unit = ()
                    |""".stripMargin
    withTempFile(content) { path =>
      // Verify that such malformed/unqualified symbol references are recognized as valid (Predef.print)
      val broken = ScaladocChecker.findBrokenLinks(path.getParent)
      val urls = broken.map(_._3)
      urls should not contain ("Predef.print(x:Any")
    }
  }

  it should "resolve fully qualified type references like scala.Product22" in {
    val tempDir = Files.createTempDirectory("product22-test")
    try
      val product22File = tempDir.resolve("Product22.scala")
      val tuple22File = tempDir.resolve("Tuple22.scala")
      Files.writeString(product22File, """package scala
                                         |
                                         |/** Product22 is a Cartesian product of 22 components. */
                                         |trait Product22 extends Product
                                         |""".stripMargin)
      Files.writeString(tuple22File, """package scala
                                         |
                                         |/** A tuple of 22 elements; the canonical representation of a [[scala.Product22]]. */
                                         |case class Tuple22() extends Product22
                                         |""".stripMargin)
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)
      // scala.Product22 should be recognized as a valid type, not a member reference
      urls should not contain ("scala.Product22")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve package references like scala.annotation.meta" in {
    val tempDir = Files.createTempDirectory("annotation-meta-test")
    try
      val setterFile = tempDir.resolve("setter.scala")
      Files.writeString(setterFile, """package scala.annotation.meta
                                        |
                                        |import scala.language.`2.13`
                                        |
                                        |/**
                                        | * Consult the documentation in package [[scala.annotation.meta]].
                                        | */
                                        |final class setter extends scala.annotation.StaticAnnotation
                                        |""".stripMargin)
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)
      // scala.annotation.meta should be recognized as a valid package
      urls should not contain ("scala.annotation.meta")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve package references defined via package object like scala.math" in {
    val tempDir = Files.createTempDirectory("scala-math-test")
    try
      val pkgFile = tempDir.resolve("package.scala")
      val equivFile = tempDir.resolve("Equiv.scala")
      Files.writeString(pkgFile, """package scala
                                    |
                                    |/** The package object `scala.math` contains methods for performing basic numeric operations. */
                                    |package object math {
                                    |  // ... math methods here
                                    |}
                                    |""".stripMargin)
      Files.writeString(equivFile, """package scala
                                       |package math
                                       |
                                       |/** Equivalence relations for types that support it.
                                       | *  See also [[scala.math]].
                                       | */
                                       |trait Equiv[T]
                                       |""".stripMargin)
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)
      // scala.math should be recognized as a valid package (defined via package object)
      urls should not contain ("scala.math")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve Java class references using external mappings" in {
    val tempDir = Files.createTempDirectory("java-external-test")
    try
      val content = """package test
                      |
                      |/** Uses Java standard library classes.
                      | *  See [[java.util.List]] and [[java.lang.String]].
                      | *  Also [[java.util.ArrayList]].
                      | */
                      |def foo(): Unit = ()
                      |""".stripMargin
      val tempFile = tempDir.resolve("Test.scala")
      Files.writeString(tempFile, content)

      // Create external mappings (simulating scaladoc format)
      val mappings = List(
        ExternalDocLink.parse(".*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/") match
          case Right(m) => m
          case Left(err) => fail(s"Failed to parse mapping: $err")
      )

      // Use the new findBrokenLinks overload with external mappings
      val broken = ScaladocChecker.findBrokenLinks(tempDir, mappings)
      val urls = broken.map(_._3)

      // Expected: these Java classes should be resolved using external mappings
      // and NOT be reported as broken
      urls should not contain ("java.util.List")
      urls should not contain ("java.lang.String")
      urls should not contain ("java.util.ArrayList")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve Java method references using external mappings" in {
    val tempDir = Files.createTempDirectory("java-method-external-test")
    try
      val content = """package test
                      |
                      |/** Uses Java standard library methods.
                      | *  See [[java.util.List.add]] and [[java.lang.String.valueOf]].
                      | */
                      |def foo(): Unit = ()
                      |""".stripMargin
      val tempFile = tempDir.resolve("Test.scala")
      Files.writeString(tempFile, content)

      // Create external mappings
      val mappings = List(
        ExternalDocLink.parse(".*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/") match
          case Right(m) => m
          case Left(err) => fail(s"Failed to parse mapping: $err")
      )

      // Use the new findBrokenLinks overload with external mappings
      val broken = ScaladocChecker.findBrokenLinks(tempDir, mappings)
      val urls = broken.map(_._3)

      // Expected: these Java methods should be resolved using external mappings
      urls should not contain ("java.util.List.add")
      urls should not contain ("java.lang.String.valueOf")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve unqualified references to methods within the same class" in {
    val content = """package test
                    |
                    |/** A test class with multiple methods.
                    | *
                    | *  See [[foo]] and [[bar]] for more details.
                    | */
                    |class MyClass:
                    |  /** Does something. */
                    |  def foo(): Unit = ()
                    |
                    |  /** Does something else. */
                    |  def bar(): Unit = ()
                    |""".stripMargin
    val tempDir = Files.createTempDirectory("unqualified-ref-test")
    try
      val tempFile = tempDir.resolve("Test.scala")
      Files.writeString(tempFile, content)

      // Unqualified references to methods within the same class should be resolved
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: foo and bar should not be reported as broken
      urls should not contain ("foo")
      urls should not contain ("bar")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve member references using # notation (scala.Option#flatten)" in {
    val content = """package test
                    |
                    |/** Test example.
                    | *
                    | *  @example [[scala.Option#flatten]]
                    | *  @example [[scala.collection.immutable.List#map]]
                    | */
                    |def foo(): Unit = ()
                    |""".stripMargin
    val tempDir = Files.createTempDirectory("hash-notation-test")
    try
      val tempFile = tempDir.resolve("Test.scala")
      Files.writeString(tempFile, content)

      // Hash notation (Type#member) should be resolved as a member reference
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: scala.Option#flatten and scala.collection.immutable.List#map should not be reported as broken
      urls should not contain ("scala.Option#flatten")
      urls should not contain ("scala.collection.immutable.List#map")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve nested objects (scala.math.Ordering.Implicits)" in {
    val content = """package test
                    |
                    |/** Test example.
                    | *
                    | *  See [[scala.math.Ordering.Implicits]].
                    | */
                    |def foo(): Unit = ()
                    |""".stripMargin
    val tempDir = Files.createTempDirectory("nested-object-test")
    try
      val tempFile = tempDir.resolve("Test.scala")
      Files.writeString(tempFile, content)

      // Nested objects should be resolved (they're represented with $ in JVM)
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: scala.math.Ordering.Implicits should not be reported as broken
      urls should not contain ("scala.math.Ordering.Implicits")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve deeply nested objects (scala.language.experimental.captureChecking)" in {
    val tempDir = Files.createTempDirectory("deeply-nested-test")
    try
      // Create a structure similar to scala.language
      val langFile = tempDir.resolve("language.scala")
      Files.writeString(langFile, """package scala

/** The `scala.language` object enables language features. */
object language:
  /** Experimental language features. */
  object experimental:
    /** Capture checking feature. */
    object captureChecking
""")

      // Create a test file that references the deeply nested object
      val testFile = tempDir.resolve("Test.scala")
      Files.writeString(testFile, """package test

/** Test example.
 *
 *  See [[scala.language.experimental.captureChecking]].
 */
def foo(): Unit = ()
""")

      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: scala.language.experimental.captureChecking should not be reported as broken
      urls should not contain ("scala.language.experimental.captureChecking")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve member references to implicit class methods (scala.Function1.UnliftOps.unlift)" in {
    val tempDir = Files.createTempDirectory("unliftops-test")
    try
      // Create a structure similar to scala.Function1 with UnliftOps
      val function1File = tempDir.resolve("Function1.scala")
      Files.writeString(function1File, """package scala

object Function1:

  implicit final class UnliftOps[A, B] private[Function1](private val f: A => Option[B]) extends AnyVal {
    /** Converts an optional function to a partial function.
     *
     *  @example Unlike [[Function.unlift]], this [[UnliftOps.unlift]] method can be used in extractors.
     */
    def unlift: PartialFunction[A, B] = ???
  }
""")

      // Create a test file that references the method
      val testFile = tempDir.resolve("Test.scala")
      Files.writeString(testFile, """package test

/** Test example.
 *
 *  See [[scala.Function1.UnliftOps.unlift]].
 */
def foo(): Unit = ()
""")

      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: scala.Function1.UnliftOps.unlift should not be reported as broken
      urls should not contain ("scala.Function1.UnliftOps.unlift")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve unqualified member references in the same file (UnliftOps.unlift)" in {
    val tempDir = Files.createTempDirectory("unqualified-ref-test")
    try
      // Create a structure similar to scala.Function1 with UnliftOps
      val function1File = tempDir.resolve("Function1.scala")
      Files.writeString(function1File, """package scala

object Function1 {

  implicit final class UnliftOps[A, B] private[Function1](private val f: A => Option[B]) extends AnyVal {
    /** Converts an optional function to a partial function.
     *
     *  @example Unlike [[Function.unlift]], this [[UnliftOps.unlift]] method can be used in extractors.
     */
    def unlift: PartialFunction[A, B] = Function.unlift(f)
  }

}
""")

      // Check the Function1.scala file itself
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: UnliftOps.unlift should not be reported as broken
      urls should not contain ("UnliftOps.unlift")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve member references to classes inside companion objects (scala.collection.MapOps.LazyKeySet)" in {
    val tempDir = Files.createTempDirectory("lazykeyset-test")
    try
      // Create a structure similar to scala.collection.Map with MapOps
      val mapFile = tempDir.resolve("Map.scala")
      Files.writeString(mapFile, """package scala.collection

object MapOps {

  /** The implementation class of the set returned by `keySet`, for pure maps. */
  private[collection] class LazyKeySet[K, +V](mp: MapOps[K, V]) extends AnyRef {
    def iterator: Iterator[K] = ???
  }

  /** The implementation class of the set returned by `keySet`, for impure maps. */
  private[collection] class StrictKeySet[K, +V](mp: MapOps[K, V]) extends AnyRef {
    def iterator: Iterator[K] = ???
  }
}

trait MapOps[K, V] {
  /** A generic trait that is reused by keyset implementations.
   *
   *  See [[MapOps.LazyKeySet]] for a version that lazily captures the map.
   */
  protected trait GenKeySet {
    def iterator: Iterator[K]
  }
}
""")

      // Create a test file that references the class
      val testFile = tempDir.resolve("Test.scala")
      Files.writeString(testFile, """package test

/** Test example.
 *
 *  See [[scala.collection.MapOps.LazyKeySet]].
 */
def foo(): Unit = ()
""")

      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: scala.collection.MapOps.LazyKeySet should not be reported as broken
      urls should not contain ("scala.collection.MapOps.LazyKeySet")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "correctly extract types with generic parameters from wikidoc links" in {
    val tempDir = Files.createTempDirectory("generic-types-test")
    try
      // Create a test file with wikidoc links containing generic types
      val testFile = tempDir.resolve("Test.scala")
      Files.writeString(testFile, """package test

/** Test example.
 *
 *  See [[AsJavaConverters.asJava[K,V](m:scala.collection.mutable.Map[K,V])*]].
 *  Also see [[AsJavaConverters.asJava[K,V](m:scala.collection.Map[K,V])*]].
 *  And see [[scala.collection.mutable.Map]] which should be valid.
 */
def foo(): Unit = ()
""")

      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)
      val errors = broken.map(_._4)

      // Expected: The type extraction should work correctly, so errors should be about the member, not truncated types
      // The issue was that types like "scala.collection.mutable.Map[K,V]" were being cut off at "["
      // Now they should be extracted correctly
      errors should not contain ("Type not found: scala.collection.mutable.Map[K")
      errors should not contain ("Type not found: scala.collection.Map[K")
      
      // The valid type should not be reported as broken
      urls should not contain ("scala.collection.mutable.Map")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve references with omitted scala. prefix" in {
    val tempDir = Files.createTempDirectory("omitted-prefix-test")
    try
      // Create a test file that mimics the actual structure
      val javaConvertersFile = tempDir.resolve("JavaConverters.scala")
      Files.writeString(javaConvertersFile, """package scala.collection

object JavaConverters {
  /** Provides conversions from Scala collections to Java collections. */
  def asScala: Int = ???
}
""")

      // Create a test file that references the object without scala. prefix
      val testFile = tempDir.resolve("Test.scala")
      Files.writeString(testFile, """package test

/** Test example.
 *
 *  See [[collection.JavaConverters]].
 */
def foo(): Unit = ()
""")

      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: collection.JavaConverters should be resolved to scala.collection.JavaConverters
      // and NOT be reported as broken
      urls should not contain ("collection.JavaConverters")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }
  "ScaladocChecker" should "resolve references with ! separator between type and member signature" in {
    val tempDir = Files.createTempDirectory("bang-separator-test")
    try
      // Create IterableOps with sizeCompare method
      val iterableOpsFile = tempDir.resolve("IterableOps.scala")
      Files.writeString(iterableOpsFile, """package scala.collection

trait IterableOps[A, CC[_], C] {

  /** Compares the size of this collection to the size of another `Iterable`.
   *
   *  @param   that the `Iterable` whose size is compared with this $coll's size.
   *  @return  A value `x` where
   */
  def sizeCompare(that: IterableOps[_, AnyConstr, ?]): Int = ???

  /** Compares the size of this collection to the size of a number.
   *
   *  @param   otherSize the size to compare with
   *  @return  A value `x` where
   */
  def sizeCompare(otherSize: Int): Int = ???
}
""")

      // Create a test file that references IterableOps.sizeCompare using ! separator
      val testFile = tempDir.resolve("Test.scala")
      Files.writeString(testFile, """package test

/** Test example.
 *
 *  See [[scala.collection.IterableOps!.sizeCompare(Int):Int*]].
 */
def foo(): Unit = ()
""")

      // Use the default checker (reflection + source lookup) to validate symbol resolution.
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: The link with ! separator should be resolved correctly
      urls should not contain ("scala.collection.IterableOps!.sizeCompare(Int):Int*")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  "ScaladocChecker" should "resolve package references like scala.util.hashing" in {
    val tempDir = Files.createTempDirectory("package-ref-test")
    try
      // Create the directory structure for scala.util.hashing
      val hashingDir = tempDir.resolve("scala/util/hashing")
      Files.createDirectories(hashingDir)

      // Create a file in the hashing package using package chaining
      val hashingFile = hashingDir.resolve("Hashing.scala")
      Files.writeString(hashingFile, """package scala
package util.hashing

/** Test trait in hashing package */
trait Hashing
""")

      // Create a test file that references scala.util.hashing package
      val testFile = tempDir.resolve("Test.scala")
      Files.writeString(testFile, """package test

/** Test example.
 *
 *  See [[scala.util.hashing]].
 */
def foo(): Unit = ()
""")

      // Use the default checker to validate symbol resolution
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: The package reference should not be reported as broken
      // (packages are valid references in Scaladoc)
      urls should not contain ("scala.util.hashing")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  "ScaladocChecker" should "resolve unqualified member references within the same parent package (javaapi.FutureConverters)" in {
    val tempDir = Files.createTempDirectory("unqualified-subpkg-test")
    try
      // Create the directory structure for scala.jdk
      val jdkDir = tempDir.resolve("scala/jdk")
      Files.createDirectories(jdkDir)

      // Create a file in the jdk package that references javaapi.FutureConverters
      val jdkFile = jdkDir.resolve("FutureConverters.scala")
      Files.writeString(jdkFile, """package scala.jdk

/** This object provides extension methods that convert between Scala [[scala.concurrent.Future]] and Java
 * [[java.util.concurrent.CompletionStage]]
 *
 * When writing Java code, use the explicit conversion methods defined in
 * [[javaapi.FutureConverters]] instead.
 */
object FutureConverters
""")

      // Create the directory structure for scala.jdk.javaapi
      val javaapiDir = tempDir.resolve("scala/jdk/javaapi")
      Files.createDirectories(javaapiDir)

      // Create a file in the javaapi package
      val javaapiFile = javaapiDir.resolve("FutureConverters.scala")
      Files.writeString(javaapiFile, """package scala.jdk.javaapi

/** This object provides conversion methods for Java code. */
object FutureConverters
""")

      // Use the default checker to validate symbol resolution
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      // Expected: The unqualified reference should be resolved correctly
      // javaapi.FutureConverters in scala.jdk package should resolve to scala.jdk.javaapi.FutureConverters
      urls should not contain ("javaapi.FutureConverters")
    finally
      try Files.list(tempDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve scala.BigInt and scala.BigDecimal" in {
    val tempDir = Files.createTempDirectory("todowriter-link-test")
    val scalaDir = tempDir.resolve("scala")
    try
      Files.createDirectories(scalaDir)
      Files.writeString(scalaDir.resolve("BigInt.scala"), "package scala\n\nclass BigInt\n")
      Files.writeString(scalaDir.resolve("BigDecimal.scala"), "package scala\n\nclass BigDecimal\n")
      
      ScaladocChecker.clearCaches()
      ScaladocChecker.symbolExistsInSource(tempDir, "scala.BigInt") should be(true)
      ScaladocChecker.clearCaches()
      ScaladocChecker.symbolExistsInSource(tempDir, "scala.BigDecimal") should be(true)
    finally
      try Files.list(scalaDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(scalaDir) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }

  it should "resolve ExecutionContext.Implicits.global" in {
    val tempDir = Files.createTempDirectory("todowriter-link-test")
    val concurrentDir = tempDir.resolve("scala/concurrent")
    try
      Files.createDirectories(concurrentDir)

      // Create ExecutionContext.scala with the nested object structure
      val ecContent = """package scala.concurrent

object ExecutionContext {
  object Implicits {
    implicit val global: ExecutionContext = ???
  }
}
"""
      Files.writeString(concurrentDir.resolve("ExecutionContext.scala"), ecContent)

      // Create a test file that references ExecutionContext.Implicits.global
      val testFile = concurrentDir.resolve("Test.scala")
      Files.writeString(testFile, """package scala.concurrent

/** See [[ExecutionContext.Implicits.global]] */
def foo(): Unit = ()
""")

      ScaladocChecker.clearCaches()
      val broken = ScaladocChecker.findBrokenLinks(tempDir)
      val urls = broken.map(_._3)

      urls should not contain ("ExecutionContext.Implicits.global")
    finally
      try Files.list(concurrentDir).forEach(p => try Files.deleteIfExists(p) catch case _: Throwable => ()) catch case _: Throwable => ()
      try Files.deleteIfExists(concurrentDir) catch case _: Throwable => ()
      try Files.deleteIfExists(tempDir) catch case _: Throwable => ()
  }
