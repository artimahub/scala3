package scaladoc_todo_marker

import org.scalatest.funspec.AnyFunSpec
import java.nio.file.Paths

class ScaladocTodoMarkerSpec extends AnyFunSpec:

  describe("ScaladocTodoMarker.parseArgs") {
    it("parseArgs: defaults") {
      val cfg = ScaladocTodoMarker.parseArgs(Array.empty)
      assert(cfg.dryRun)
      assert(cfg.root == Paths.get("."))
      assert(cfg.excludes.isEmpty)
    }

    it("--apply sets dryRun to false") {
      val cfg = ScaladocTodoMarker.parseArgs(Array("--apply"))
      assert(!cfg.dryRun)
    }

    it("--dry-run overrides apply to true") {
      val cfg = ScaladocTodoMarker.parseArgs(Array("--apply", "--dry-run"))
      assert(cfg.dryRun)
    }

    it("--root sets root path") {
      val cfg = ScaladocTodoMarker.parseArgs(Array("--root", "some/dir"))
      assert(cfg.root == Paths.get("some/dir"))
    }

    it("--exclude parses comma separated values") {
      val cfg = ScaladocTodoMarker.parseArgs(Array("--exclude", "gen,tmp, dist"))
      assert(cfg.excludes == Seq("gen", "tmp", "dist"))
    }

    it("unknown args are ignored and other flags still parsed") {
      val cfg = ScaladocTodoMarker.parseArgs(Array("--foo", "--root", "r"))
      assert(cfg.root == Paths.get("r"))
    }
  }