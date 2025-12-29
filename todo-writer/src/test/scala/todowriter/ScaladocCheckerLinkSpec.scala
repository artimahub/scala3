
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