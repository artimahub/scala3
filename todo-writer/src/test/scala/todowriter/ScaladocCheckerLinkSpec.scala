package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
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
        def checker(url: String): Option[String] =
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
        def checker(url: String): Option[String] =
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
      def checker(url: String): Option[String] =
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
      def checker(url: String): Option[String] =
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
      def checker(url: String): Option[String] =
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
      Files.writeString(file1, "package com.example\n\nclass Foo\n")
      Files.writeString(file2, "object Bar\n")
      ScaladocChecker.symbolExistsInSource(tempDir, "com.example.Foo") should be(true)
      ScaladocChecker.symbolExistsInSource(tempDir, "Bar") should be(true)
      ScaladocChecker.symbolExistsInSource(tempDir, "com.example.Missing") should be(false)
      // Also verify case class and trait detection
      val file3 = tempDir.resolve("Baz.scala")
      Files.writeString(file3, "package pkg\n\ncase class Baz(x: Int)\ntrait Qux\n")
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
      def checker(url: String): Option[String] = Some("HTTP 404")
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
      def checker(url: String): Option[String] =
        if url.endsWith("/good") then None else Some("HTTP 404")
      val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
      // The code fence should not produce a link; only the markdown link should be considered.
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
      def checker(url: String): Option[String] = Some("HTTP 404")
      val broken = ScaladocChecker.findBrokenLinks(path.getParent, checker)
      val urls = broken.map(_._3)
      // Only the explicit http link should be reported; the triple-brace code should be ignored.
      urls.count(_.startsWith("http://")) should be (1)
    }
  }

  it should "resolve Predef.print method symbol (failing test)" in {
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

  it should "resolve ExecutionContext$.global symbol (failing test)" in {
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

  it should "resolve object/member symbols like scala.concurrent.duration.Duration.Inf (failing test)" in {
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

  it should "handle unqualified/malformed method references like Predef.print(x:Any) (failing test)" in {
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