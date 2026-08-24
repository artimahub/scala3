package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Regression tests: running the tool against a project root must not scan
 *  build-output or metadata directories (e.g. sbt `target/`). Otherwise the
 *  tool rewrites downloaded/generated sources (like the Scala 2.13 sources
 *  under `library/target/scala2-library/sources`) and breaks the build that
 *  compiles them.
 */
class ScaladocStubSkipsTargetDirSpec extends AnyFlatSpec with Matchers:

  private def deleteRecursively(p: Path): Unit =
    if Files.isDirectory(p) then
      val it = Files.list(p)
      try
        it.forEach(deleteRecursively)
      finally it.close()
    Files.deleteIfExists(p)

  private def withTempDir(test: Path => Unit): Unit =
    val tempDir = Files.createTempDirectory("todowriter-skip-target")
    try test(tempDir)
    finally deleteRecursively(tempDir)

  it should "skip .scala files under target/ and .git/" in {
    withTempDir { root =>
      val src = Files.writeString(root.resolve("Main.scala"), "object Main\n")
      Files.createDirectories(root.resolve("target/scala2-library/sources"))
      val generated = Files.writeString(
        root.resolve("target/scala2-library/sources/Downloaded.scala"),
        "object Downloaded\n"
      )
      Files.createDirectories(root.resolve(".git"))
      val gitFile = Files.writeString(root.resolve(".git/config.scala"), "object Git\n")

      ScaladocChecker.clearCaches()
      val found = ScaladocChecker.findScalaFiles(root).map(_.getFileName.toString)

      found should contain("Main.scala")
      found should not contain "Downloaded.scala"
      found should not contain "config.scala"
    }
  }

  it should "still scan a source file when the scanned root itself is named target" in {
    withTempDir { outer =>
      val targetDir = outer.resolve("target")
      Files.createDirectories(targetDir)
      val src = Files.writeString(targetDir.resolve("Real.scala"), "object Real\n")
      val subTarget = targetDir.resolve("target")
      Files.createDirectories(subTarget)
      val generated = Files.writeString(subTarget.resolve("Nested.scala"), "object Nested\n")

      ScaladocChecker.clearCaches()
      val found = ScaladocChecker.findScalaFiles(targetDir).map(_.getFileName.toString)

      found should contain("Real.scala")
      found should not contain "Nested.scala"
    }
  }

  it should "skip target/ directories when checking a directory" in {
    withTempDir { root =>
      Files.writeString(root.resolve("Top.scala"), "object Top\n")
      Files.createDirectories(root.resolve("target"))
      Files.writeString(root.resolve("target/Bottom.scala"), "object Bottom\n")

      ScaladocChecker.clearCaches()
      val results = ScaladocChecker.checkDirectory(root)

      results.map(_.path) should have size 1
      results.head.path should endWith("Top.scala")
    }
  }
