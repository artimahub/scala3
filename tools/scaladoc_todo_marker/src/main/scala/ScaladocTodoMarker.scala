package scaladoc_todo_marker

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.nio.charset.StandardCharsets
import scala.util.matching.Regex
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object ScaladocTodoMarker:
  private val declRegex: Regex = raw"\b(class|object|trait|def|val|var)\b".r

  case class Config(root: Path = Paths.get("."), dryRun: Boolean = true, excludes: Seq[String] = Seq.empty)

  def parseArgs(args: Array[String]): Config =
    var cfg = Config()
    var i = 0
    while i < args.length do
      args(i) match
        case "--apply" => cfg = cfg.copy(dryRun = false); i += 1
        case "--dry-run" => cfg = cfg.copy(dryRun = true); i += 1
        case "--root" =>
          i += 1
          if i < args.length then cfg = cfg.copy(root = Paths.get(args(i)))
          i += 1
        case "--exclude" =>
          i += 1
          if i < args.length then cfg = cfg.copy(excludes = args(i).split(",").map(_.trim).filter(_.nonEmpty).toSeq)
          i += 1
        case _ => i += 1
    cfg

  def isExcluded(path: Path, excludes: Seq[String]): Boolean =
    val s = path.toString
    excludes.exists(p => s.contains(p))

  def findScalaFiles(root: Path, excludes: Seq[String]): Seq[Path] =
    if !Files.exists(root) then Seq.empty
    else
      Files.walk(root).iterator().asScala
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".scala"))
        .filterNot(p => isExcluded(p, excludes))
        .toSeq

  def leadingWhitespace(s: String): String =
    s.takeWhile(c => c == ' ' || c == '\t')

  def isModifierPresent(prefix: String, mod: String): Boolean =
    ("\\b" + Regex.quote(mod) + "\\b").r.findFirstIn(prefix).isDefined

  def processFile(path: Path): Option[(Path, Seq[String])] =
    val bytes = Files.readAllBytes(path)
    val content = new String(bytes, StandardCharsets.UTF_8)
    val lines = content.split("\n", -1).toBuffer

    val FORWARD_WINDOW = 16
    val BACKWARD_WINDOW = 16

    // Helper: is there a proper Scaladoc block immediately (allowing small gaps) above declIdx
    def hasProperScaladocAbove(declIdx: Int, lookback: Int = 400): Boolean =
      var k = declIdx - 1
      var steps = 0
      var foundStart = -1
      while k >= 0 && steps < lookback && foundStart < 0 do
        if lines(k).contains("/**") then foundStart = k
        else
          val tt = lines(k).trim
          if tt.nonEmpty && !tt.startsWith("//") && !tt.startsWith("@") && !tt.startsWith("*") && !tt.startsWith("/*") && tt != "*/" then
            return false
          else
            k -= 1
            steps += 1
      if foundStart < 0 then false
      else
        var kk = foundStart
        var foundEnd = -1
        while kk < declIdx && foundEnd < 0 do
          if lines(kk).contains("*/") then foundEnd = kk
          kk += 1
        if foundEnd < 0 || foundEnd >= declIdx then false
        else
          var m = foundEnd + 1
          while m < declIdx do
            val tt = lines(m).trim
            if tt.isEmpty || tt.startsWith("@") || tt.startsWith("//") || tt.startsWith("*") then m += 1
            else return false
          true

    // Helper: find nearest enclosing container (class/object/trait) for a declaration at declIdx
    def findEnclosingContainerIndex(declIdx: Int): Option[Int] =
      val currIndent = leadingWhitespace(lines(declIdx)).length
      if currIndent == 0 then None
      else
        var k = declIdx - 1
        while k >= 0 do
          val lk = lines(k)
          val lkTrim = lk.trim
          if lkTrim.isEmpty || lkTrim.startsWith("//") || lkTrim.startsWith("@") then k -= 1
          else
            declRegex.findFirstMatchIn(lk) match
              case Some(mm) =>
                val kw = mm.group(1)
                if kw == "class" || kw == "object" || kw == "trait" then
                  val indentK = leadingWhitespace(lk).length
                  if indentK < currIndent then return Some(k)
                  else k -= 1
                else k -= 1
              case None => k -= 1
        None

    // Cleanup: remove prior "TODO FILL IN" lines that are followed by a proper Scaladoc+decl within a window
    val toRemove = mutable.BitSet()
    var idx = 0
    while idx < lines.length do
      val t = lines(idx)
      if t.contains("TODO FILL IN") then
        var found = false
        var k = idx + 1
        var steps = 0
        while k < lines.length && steps < FORWARD_WINDOW && !found do
          if lines(k).contains("/**") then
            var kk = k
            var docEnd = -1
            while kk < lines.length && kk < k + FORWARD_WINDOW && docEnd < 0 do
              if lines(kk).contains("*/") then docEnd = kk
              kk += 1
            val searchFrom = if docEnd >= 0 then docEnd + 1 else k
            var mm = searchFrom
            var steps2 = 0
            while mm < lines.length && steps2 < FORWARD_WINDOW && !found do
              if declRegex.findFirstIn(lines(mm)).isDefined then found = true
              mm += 1
              steps2 += 1
          k += 1
          steps += 1
        if !found then
          k = idx - 1
          steps = 0
          while k >= 0 && steps < BACKWARD_WINDOW && !found do
            if lines(k).contains("/**") then
              var kk = k
              var docEnd = -1
              while kk < lines.length && kk < k + BACKWARD_WINDOW && docEnd < 0 do
                if lines(kk).contains("*/") then docEnd = kk
                kk += 1
              val searchFrom = if docEnd >= 0 then docEnd + 1 else k
              var mm = searchFrom
              var steps2 = 0
              while mm < lines.length && steps2 < FORWARD_WINDOW && !found do
                if declRegex.findFirstIn(lines(mm)).isDefined then found = true
                mm += 1
                steps2 += 1
            k -= 1
            steps += 1
        if found then toRemove += idx
      idx += 1

    if toRemove.nonEmpty then
      val cleaned = ListBuffer.empty[String]
      for i <- 0 until lines.length do
        if !toRemove.contains(i) then cleaned += lines(i)
      lines.clear(); lines ++= cleaned.toBuffer

    val edits = ListBuffer.empty[(Int, String)]

    var inBlockComment = false
    var i = 0
    while i < lines.length do
      val line = lines(i)
      val trimmed = line.trim

      if !inBlockComment && trimmed.contains("/*") && !trimmed.contains("*/") then inBlockComment = true
      val isBlockCommentLine = (trimmed.contains("/*") && trimmed.contains("*/")) || inBlockComment
      if inBlockComment && trimmed.contains("*/") then inBlockComment = false

      if !isBlockCommentLine && !trimmed.startsWith("//") then
        declRegex.findFirstMatchIn(line) match
          case Some(m) =>
            val kw = m.group(1)
            val kwStart = m.start
            val prefix = line.substring(0, kwStart)
            if !prefix.contains("=") then
              val hasPrivate = isModifierPresent(prefix, "private") || prefix.contains("private") || line.contains(" private ")
              val hasProtected = isModifierPresent(prefix, "protected") || prefix.contains("protected") || line.contains(" protected ")

              var isTarget = false

              kw match
                case "val" | "var" =>
                  if hasProtected then isTarget = true
                  else
                    val currIndent = leadingWhitespace(line).length
                    var k = i - 1
                    var insideMethod = false
                    var foundMemberContainer = false
                    while k >= 0 && !insideMethod && !foundMemberContainer do
                      val lk = lines(k); val lkTrim = lk.trim
                      if lkTrim.isEmpty || lkTrim.startsWith("//") || lkTrim.startsWith("@") then k -= 1
                      else
                        val indentK = leadingWhitespace(lk).length
                        val t = lkTrim
                        if t.startsWith("def ") && indentK < currIndent then insideMethod = true
                        else if t.startsWith("class ") || t.startsWith("object ") || t.startsWith("trait ") then foundMemberContainer = true
                        else k -= 1
                    if !insideMethod && (foundMemberContainer || leadingWhitespace(line).isEmpty) then isTarget = !hasPrivate
                case "def" =>
                  if trimmed.startsWith("def this(") then isTarget = !hasPrivate
                  else
                    val currIndent = leadingWhitespace(line).length
                    var k = i - 1
                    var insideMethod = false
                    while k >= 0 && !insideMethod do
                      val lkTrim = lines(k).trim
                      if lkTrim.isEmpty || lkTrim.startsWith("//") || lkTrim.startsWith("@") then k -= 1
                      else
                        val indentK = leadingWhitespace(lines(k)).length
                        if lkTrim.startsWith("def ") && indentK < currIndent then insideMethod = true
                        else k -= 1
                    if !insideMethod then isTarget = !hasPrivate
                case "class" | "object" | "trait" =>
                  if hasProperScaladocAbove(i) then isTarget = false
                  else
                    var hasNonPrivateMember = false
                    var hasNonPrivateMemberWithDoc = false
                    var kk = i + 1
                    var steps = 0
                    val MAX_LOOKAHEAD = 200
                    while kk < lines.length && steps < MAX_LOOKAHEAD do
                      val l = lines(kk)
                      declRegex.findFirstMatchIn(l) match
                        case Some(mm) =>
                          val kw2 = mm.group(1)
                          val kwStart2 = mm.start
                          val prefix2 = l.substring(0, kwStart2)
                          val hasPrivate2 = isModifierPresent(prefix2, "private") || prefix2.contains("private") || l.contains(" private ")
                          if !hasPrivate2 then
                            hasNonPrivateMember = true
                            if hasProperScaladocAbove(kk) then hasNonPrivateMemberWithDoc = true
                        case None => ()
                      kk += 1
                      steps += 1
                    isTarget = !hasPrivate && hasNonPrivateMember && !hasNonPrivateMemberWithDoc
                case _ =>
                  isTarget = !hasPrivate

              // If enclosing container has Scaladoc, skip member placeholders except for
              // secondary constructors and simple val/var constants (keep marking vals/vars).
              if isTarget then
                findEnclosingContainerIndex(i) match
                  case Some(containerIdx) if hasProperScaladocAbove(containerIdx) && !(kw == "def" && trimmed.startsWith("def this(")) && !(kw == "val" || kw == "var") =>
                    // container has Scaladoc and this member is neither a secondary constructor nor a val/var -> skip
                    isTarget = false
                  case _ => ()

              if isTarget then
                var start = i
                var cont = true
                while start > 0 && cont do
                  val prev = lines(start - 1).trim
                  if prev.isEmpty || prev.startsWith("@") || prev.startsWith("//") || prev.startsWith("*") || prev.startsWith("/*") then start -= 1
                  else if prev.startsWith("package") || prev.startsWith("import") then cont = false
                  else cont = false

                var j = start
                var betweenHasDoc = false
                while j < i && !betweenHasDoc do
                  if lines(j).contains("/**") then betweenHasDoc = true
                  j += 1

                val aboveHasDoc = hasProperScaladocAbove(i)
                val nearbyHasDoc = hasProperScaladocAbove(i, 24)

                var alreadyTodo = false
                if start > 0 then
                  val lookIdx = start - 1
                  if lines(lookIdx).contains("TODO FILL IN") then alreadyTodo = true

                val lastOpen = (0 until i).reverse.find(k => lines(k).contains("/*")).getOrElse(-1)
                val lastClose = (0 until i).reverse.find(k => lines(k).contains("*/")).getOrElse(-1)
                val inOrAfterBlock = lastOpen > lastClose

                val docLikeBetween = (start until i).exists { k =>
                  val t = lines(k).trim
                  t.startsWith("*") || t.startsWith("/**") || t.startsWith("/*") ||
                  t.contains("@param") || t.contains("@return") || t.contains("@see")
                }

                if !betweenHasDoc && !aboveHasDoc && !nearbyHasDoc && !alreadyTodo && !inOrAfterBlock && !docLikeBetween then
                  val indent = leadingWhitespace(line)
                  val insertLine = s"${indent}/** TODO FILL IN */"
                  edits += ((start, insertLine))
          case None => ()
      i += 1

    if edits.isEmpty then None
    else
      val newLines = ListBuffer.empty[String]
      var editIdx = 0
      val sorted = edits.sortBy(_._1)
      i = 0
      while i < lines.length do
        while editIdx < sorted.length && sorted(editIdx)._1 == i do
          newLines += sorted(editIdx)._2
          editIdx += 1
        newLines += lines(i)
        i += 1
      while editIdx < sorted.length do
        newLines += sorted(editIdx)._2
        editIdx += 1
      Some((path, newLines.toSeq))

  def applyChange(path: Path, newLines: Seq[String]): Unit =
    val tmp = path.resolveSibling(path.getFileName.toString + ".tmp")
    Files.write(tmp, newLines.asJava, StandardCharsets.UTF_8)
    Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)

  def run(args: Array[String]): Int =
    val cfg = parseArgs(args)
    val files = findScalaFiles(cfg.root, cfg.excludes)
    var totalFiles = 0
    var changedFiles = 0

    for f <- files do
      totalFiles += 1
      processFile(f) match
        case Some((path, newLines)) =>
          val rel = cfg.root.relativize(path).toString
          changedFiles += 1
          if cfg.dryRun then println(s"[DRY] would update: $rel")
          else
            applyChange(path, newLines)
            println(s"[APPLY] updated: $rel")
        case None => ()

    println(s"Scanned $totalFiles .scala files; ${changedFiles} files need scaladoc placeholders.")
    if cfg.dryRun then println("Run with --apply to write changes.")
    0

object Main:
  def main(args: Array[String]): Unit =
    if args.contains("--help") || args.contains("-h") then
      Console.err.println(
        """Scaladoc Checker
          |Finds public or protected declarations missing Scaladoc and inserts /** TODO FILL IN */ above them.
          |
          |Usage:
          |  sbt "runMain scaladoc_todo_marker.Main -- [--dry-run] [--apply] [--root PATH] [--exclude pat1,pat2]"
          |
          |Examples:
          |  sbt "runMain scaladoc_todo_marker.Main -- --dry-run"
          |  sbt "runMain scaladoc_todo_marker.Main -- --apply --root library --exclude generated,tmp"
          |""".stripMargin)
      System.exit(0)

    val exit = ScaladocTodoMarker.run(args)
    System.exit(exit)