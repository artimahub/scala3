package todowriter

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path, Paths}

/** Reproducible counter for the Scala 3 standard-library Scaladoc effort
 *  described in the accompanying blog post.
 *
 *  It reports two independent numbers and a combined total:
 *
 *    1. How many `@param`, `@tparam`, and `@return` tags were ADDED across the
 *       pull requests submitted to scala/scala3. The PR numbers are listed in
 *       source below; their diffs are downloaded from GitHub and the additions
 *       counted directly, so the figure can be reproduced by anyone.
 *
 *    2. How many TODO placeholders `todo-writer --only-undocumented` would
 *       insert for declarations in the Scala 3 standard library that have no
 *       Scaladoc comment at all. This runs the same checker/fixer the tool uses,
 *       in memory (nothing is written), over the library `src` directories.
 *
 *  Run it from the `todo-writer` directory:
 *  {{{
 *  sbt "runMain todowriter.Counter"
 *  }}}
 *
 *  Set the `GITHUB_TOKEN` environment variable to raise GitHub's rate limit if
 *  you run it repeatedly (not required for a single run).
 */
object Counter:

  // ---------------------------------------------------------------------------
  // The pull requests whose additions we count.
  //
  // These are exactly the PRs that filled in missing @param/@tparam/@return
  // Scaladoc tags. Deliberately EXCLUDED: style-only / wikidoc-migration PRs
  // (#24754, #25113), the `& caps.Pure` bounds PR (#26224, no tags), and the
  // superseded-and-closed PRs (#25065, #25268, #25382; #25382 was replaced by
  // #25996).
  // ---------------------------------------------------------------------------

  /** First pass over parts 1-12 of the library (all merged). */
  val firstPassMerged: List[Int] =
    List(25371, 25372, 25373, 25374, 25375, 25376, 25377, 25378, 25379, 25380, 25381, 25996)

  /** Second pass, "filled in tags that were missed last time" (merged). */
  val secondPassMerged: List[Int] =
    List(26119, 26122, 26123)

  /** Second pass, still open at the time of writing. */
  val secondPassOpen: List[Int] =
    List(26120, 26121, 26124, 26125, 26126, 26127)

  /** Every PR we count, in order. */
  val pullRequests: List[Int] = firstPassMerged ++ secondPassMerged ++ secondPassOpen

  /** Scala 3 standard-library source roots scanned for undocumented
   *  declarations. We scan `src` only, never `target`, so generated Scala 2
   *  library sources are not counted. Paths are relative to the `todo-writer`
   *  directory; override by passing roots as command-line arguments.
   */
  val defaultLibraryRoots: List[String] = List("../library/src", "../library-js/src")

  /** The marker the fixer inserts for every missing piece of documentation. */
  val TodoMarker = "TODO FILL IN"

  // ---------------------------------------------------------------------------
  // 1. Counting tag additions in the submitted PRs.
  // ---------------------------------------------------------------------------

  /** Counts of the three Scaladoc tags. */
  case class TagCounts(param: Int, tparam: Int, ret: Int):
    def +(o: TagCounts): TagCounts = TagCounts(param + o.param, tparam + o.tparam, ret + o.ret)
    def total: Int = param + tparam + ret

  object TagCounts:
    val zero: TagCounts = TagCounts(0, 0, 0)

  private val http: HttpClient =
    HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()

  /** Download a PR's unified diff from GitHub. Uses the public `.diff` URL,
   *  which is not subject to the API rate limit.
   */
  def fetchDiff(pr: Int): String =
    val uri = URI.create(s"https://github.com/scala/scala3/pull/$pr.diff")
    val builder = HttpRequest.newBuilder(uri).GET()
    sys.env.get("GITHUB_TOKEN").foreach(token => builder.header("Authorization", s"Bearer $token"))
    val response = http.send(builder.build(), HttpResponse.BodyHandlers.ofString())
    if response.statusCode() != 200 then
      throw new RuntimeException(s"Failed to download diff for PR #$pr: HTTP ${response.statusCode()}")
    response.body()

  /** Count non-overlapping occurrences of `needle` in `haystack`. */
  def countOccurrences(haystack: String, needle: String): Int =
    var count = 0
    var idx = haystack.indexOf(needle)
    while idx >= 0 do
      count += 1
      idx = haystack.indexOf(needle, idx + needle.length)
    count

  /** Count tags ADDED in a diff. Only added lines (`+`, not the `+++` file
   *  header) inside `.scala` files are counted.
   */
  def countTagsInDiff(diff: String): TagCounts =
    var currentFile = ""
    var counts = TagCounts.zero
    for line <- diff.linesIterator do
      if line.startsWith("diff --git") then
        currentFile = line.split(" b/").lastOption.getOrElse("")
      else if currentFile.endsWith(".scala") && line.startsWith("+") && !line.startsWith("+++") then
        counts = counts + TagCounts(
          countOccurrences(line, "@param"),
          countOccurrences(line, "@tparam"),
          countOccurrences(line, "@return")
        )
    counts

  // ---------------------------------------------------------------------------
  // 2. Counting TODOs for completely-undocumented declarations.
  // ---------------------------------------------------------------------------

  /** Result of scanning the library source for undocumented declarations. */
  case class UndocCounts(declarations: Int, todos: Int):
    def +(o: UndocCounts): UndocCounts = UndocCounts(declarations + o.declarations, todos + o.todos)

  object UndocCounts:
    val zero: UndocCounts = UndocCounts(0, 0)

  /** Scan the given source roots and count, without writing anything, how many
   *  declarations have no Scaladoc at all and how many TODO placeholders the
   *  fixer would insert for them. This is exactly what `--only-undocumented`
   *  does, run in memory.
   */
  def countUndocumented(roots: List[String]): UndocCounts =
    var result = UndocCounts.zero
    for root <- roots do
      val path = Paths.get(root)
      if !Files.isDirectory(path) then
        throw new RuntimeException(s"Library source root not found: $root (run from the todo-writer directory)")
      // Include undocumented declarations in the scan, then keep only synthetic
      // blocks: declarations that have no Scaladoc block at all.
      val fileResults = ScaladocChecker.checkDirectory(path, skipUndocumented = false)
      for fileResult <- fileResults do
        val undocumented = fileResult.results.filter(_.scaladoc.synthetic)
        if undocumented.nonEmpty then
          val original = Files.readString(Paths.get(fileResult.path))
          val (fixed, _) = Fixer.applyFixes(original, undocumented, insertTodo = true)
          val inserted = countOccurrences(fixed, TodoMarker) - countOccurrences(original, TodoMarker)
          result = result + UndocCounts(undocumented.size, inserted)
    result

  // ---------------------------------------------------------------------------
  // Entry point.
  // ---------------------------------------------------------------------------

  def main(args: Array[String]): Unit =
    val roots = if args.nonEmpty then args.toList else defaultLibraryRoots

    println("Scala 3 standard-library Scaladoc counts")
    println("=" * 60)

    println()
    println(s"1. Tags added across ${pullRequests.size} submitted PRs (downloading diffs)...")
    println()
    var tagTotals = TagCounts.zero
    for pr <- pullRequests do
      val counts = countTagsInDiff(fetchDiff(pr))
      tagTotals = tagTotals + counts
      println(f"   PR #$pr%-6d  @param ${counts.param}%5d   @tparam ${counts.tparam}%5d   @return ${counts.ret}%5d")
    println("   " + "-" * 52)
    println(f"   TOTAL        @param ${tagTotals.param}%5d   @tparam ${tagTotals.tparam}%5d   @return ${tagTotals.ret}%5d")
    println(f"   Tags added (subtotal): ${tagTotals.total}")

    println()
    println("2. TODOs for declarations with no Scaladoc at all (--only-undocumented)...")
    println()
    val undoc = countUndocumented(roots)
    println(f"   Declarations with no Scaladoc: ${undoc.declarations}%6d")
    println(f"   TODO placeholders to insert:   ${undoc.todos}%6d")

    println()
    println("=" * 60)
    println(f"GRAND TOTAL (tags added + undocumented TODOs): ${tagTotals.total + undoc.todos}")
