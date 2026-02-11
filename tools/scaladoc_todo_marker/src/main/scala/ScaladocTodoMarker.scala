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
    @scala.annotation.tailrec
    def recur(rem: List[String], cfg: Config): Config = rem match
      case "--apply" :: tail           => recur(tail, cfg.copy(dryRun = false))
      case "--dry-run" :: tail         => recur(tail, cfg.copy(dryRun = true))
      case "--root" :: value :: tail   => recur(tail, cfg.copy(root = Paths.get(value)))
      case "--root" :: Nil             => cfg
      case "--exclude" :: value :: tail =>
        val excludes = value.split(",").map(_.trim).filter(_.nonEmpty).toSeq
        recur(tail, cfg.copy(excludes = excludes))
      case _ :: tail                   => recur(tail, cfg)
      case Nil                         => cfg
    recur(args.toList, Config())

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
    val lines = content.split("\n", -1).toList

    val FORWARD_WINDOW = 16
    val BACKWARD_WINDOW = 16

    def hasProperScaladocAbove(declIdx: Int, lookback: Int = 400): Boolean =
      @scala.annotation.tailrec
      def walkBack(k: Int, steps: Int, foundStart: Option[Int]): Boolean =
        if foundStart.isEmpty && k >= 0 && steps < lookback then
          if lines(k).contains("/**") then walkBack(k, steps, Some(k))
          else
            val tt = lines(k).trim
            if tt.nonEmpty && !tt.startsWith("//") && !tt.startsWith("@") && !tt.startsWith("*") && !tt.startsWith("/*") && tt != "*/" then false
            else walkBack(k - 1, steps + 1, foundStart)
        else foundStart match
          case None => false
          case Some(start) =>
            def walkForward(kk: Int, foundEnd: Option[Int]): Boolean =
              if kk < declIdx && foundEnd.isEmpty then
                if lines(kk).contains("*/") then walkForward(kk + 1, Some(kk))
                else walkForward(kk + 1, foundEnd)
              else foundEnd match
                case None => false
                case Some(end) =>
                  def checkBetween(m: Int): Boolean =
                    if m >= declIdx then true
                    else
                      val tt = lines(m).trim
                      if tt.isEmpty || tt.startsWith("@") || tt.startsWith("//") || tt.startsWith("*") then checkBetween(m + 1)
                      else false
                  checkBetween(end + 1)
            walkForward(start, None)
      walkBack(declIdx - 1, 0, None)

    def findEnclosingContainerIndex(declIdx: Int): Option[Int] =
      val currIndent = leadingWhitespace(lines(declIdx)).length
      if currIndent == 0 then None
      else
        def loop(k: Int): Option[Int] =
          if k < 0 then None
          else
            val lk = lines(k); val lkTrim = lk.trim
            if lkTrim.isEmpty || lkTrim.startsWith("//") || lkTrim.startsWith("@") then loop(k - 1)
            else
              declRegex.findFirstMatchIn(lk) match
                case Some(mm) =>
                  val kw = mm.group(1)
                  if kw == "class" || kw == "object" || kw == "trait" then
                    val indentK = leadingWhitespace(lk).length
                    if indentK < currIndent then Some(k) else loop(k - 1)
                  else loop(k - 1)
                case None => loop(k - 1)
        loop(declIdx - 1)

    val todoIndices = lines.zipWithIndex.collect { case (l, i) if l.contains("TODO FILL IN") => i }

    def declWithinWindow(startIdx: Int, window: Int): Boolean =
      val end = (startIdx + window).min(lines.length - 1)
      (startIdx to end).exists { j =>
        if lines(j).contains("/**") then
          val docEnd = (j to (j + FORWARD_WINDOW).min(lines.length - 1)).find(k => lines(k).contains("*/")).getOrElse(j)
          val searchFrom = docEnd + 1
          (searchFrom to (searchFrom + FORWARD_WINDOW).min(lines.length - 1)).exists(mm => declRegex.findFirstIn(lines(mm)).isDefined)
        else false
      }

    def checkBackward(idx: Int): Boolean =
      val start = (idx - BACKWARD_WINDOW).max(0)
      (start to idx - 1).exists { k =>
        if lines(k).contains("/**") then
          val docEnd = (k to (k + BACKWARD_WINDOW).min(lines.length - 1)).find(j => lines(j).contains("*/")).getOrElse(k)
          val searchFrom = docEnd + 1
          (searchFrom to (searchFrom + FORWARD_WINDOW).min(lines.length - 1)).exists(mm => declRegex.findFirstIn(lines(mm)).isDefined)
        else false
      }

    val toRemove: Set[Int] = todoIndices.filter { idx =>
      declWithinWindow(idx + 1, FORWARD_WINDOW) || checkBackward(idx - 1)
    }.toSet

    val cleanedLines = if toRemove.isEmpty then lines else lines.zipWithIndex.filterNot((_, i) => toRemove.contains(i)).map(_._1)

    val isBlockLine: List[Boolean] =
      cleanedLines.foldLeft((List.empty[Boolean], false)) { case ((acc, inBlock), line) =>
        val trimmed = line.trim
        val startsBlock = !inBlock && trimmed.contains("/*") && !trimmed.contains("*/")
        val isLine = (trimmed.contains("/*") && trimmed.contains("*/")) || inBlock || startsBlock
        val nextInBlock = (inBlock && !trimmed.contains("*/")) || startsBlock
        (acc :+ isLine, nextInBlock)
      }._1

    val edits: Seq[(Int, String)] =
      cleanedLines.zipWithIndex.foldLeft(List.empty[(Int, String)]) { case (acc, (line, i)) =>
        val trimmed = line.trim
        val isBlockCommentLine = isBlockLine(i)
        if !isBlockCommentLine && !trimmed.startsWith("//") then
          declRegex.findFirstMatchIn(line) match
            case Some(m) =>
              val kw = m.group(1)
              val kwStart = m.start
              val prefix = line.substring(0, kwStart)
              if prefix.contains("=") then acc
              else
                val hasPrivate = isModifierPresent(prefix, "private") || prefix.contains("private") || line.contains(" private ")
                val hasProtected = isModifierPresent(prefix, "protected") || prefix.contains("protected") || line.contains(" protected ")

                def isTargetForValVar: Boolean =
                  if hasProtected then true
                  else
                    val currIndent = leadingWhitespace(line).length
                    val (insideMethod, foundMemberContainer) =
                      cleanedLines.slice(0, i).reverse.dropWhile(l => l.trim.isEmpty || l.trim.startsWith("//") || l.trim.startsWith("@")).foldLeft((false, false)) {
                        case ((inMeth, found), lk) =>
                          val indentK = leadingWhitespace(lk).length
                          val t = lk.trim
                          if !inMeth && t.startsWith("def ") && indentK < currIndent then (true, found)
                          else if !found && (t.startsWith("class ") || t.startsWith("object ") || t.startsWith("trait ")) then (inMeth, true)
                          else (inMeth, found)
                      }
                    !insideMethod && (foundMemberContainer || leadingWhitespace(line).isEmpty)

                def isTargetForDef: Boolean =
                  if trimmed.startsWith("def this(") then !hasPrivate
                  else
                    val currIndent = leadingWhitespace(line).length
                    val insideMethod =
                      cleanedLines.slice(0, i).reverse.dropWhile(l => l.trim.isEmpty || l.trim.startsWith("//") || l.trim.startsWith("@")).exists { lk =>
                        val indentK = leadingWhitespace(lk).length
                        val t = lk.trim
                        t.startsWith("def ") && indentK < currIndent
                      }
                    !insideMethod && !hasPrivate

                def isTargetForContainer: Boolean =
                  if hasProperScaladocAbove(i) then false
                  else
                    val MAX_LOOKAHEAD = 200
                    val window = cleanedLines.slice(i + 1, (i + MAX_LOOKAHEAD + 1).min(cleanedLines.length))
                    val members = window.zipWithIndex.collect {
                      case (l, off) if declRegex.findFirstMatchIn(l).isDefined =>
                        val mm = declRegex.findFirstMatchIn(l).get
                        val kw2 = mm.group(1)
                        val prefix2 = l.substring(0, mm.start)
                        val hasPrivate2 = isModifierPresent(prefix2, "private") || prefix2.contains("private") || l.contains(" private ")
                        (kw2, !hasPrivate2, i + 1 + off)
                    }
                    val hasNonPrivateMember = members.exists(_._2)
                    val hasNonPrivateMemberWithDoc = members.exists { case (_, _, idx) => hasProperScaladocAbove(idx) }
                    !hasPrivate && hasNonPrivateMember && !hasNonPrivateMemberWithDoc

                val isTargetInitial = kw match
                  case "val" | "var" => isTargetForValVar
                  case "def"         => isTargetForDef
                  case "class" | "object" | "trait" => isTargetForContainer
                  case _             => !hasPrivate

                val isTargetFinal =
                  if isTargetInitial then
                    findEnclosingContainerIndex(i) match
                      case Some(containerIdx) if hasProperScaladocAbove(containerIdx) && !(kw == "def" && trimmed.startsWith("def this(")) && !(kw == "val" || kw == "var") => false
                      case _ => true
                  else false

                if isTargetFinal then
                  val start =
                    @scala.annotation.tailrec
                    def scanStart(s: Int): Int =
                      if s <= 0 then 0
                      else
                        val prev = cleanedLines(s - 1).trim
                        if prev.isEmpty || prev.startsWith("@") || prev.startsWith("//") || prev.startsWith("*") || prev.startsWith("/*") then scanStart(s - 1)
                        else if prev.startsWith("package") || prev.startsWith("import") then s
                        else s
                    scanStart(i)

                  val betweenHasDoc = cleanedLines.slice(start, i).exists(_.contains("/**"))
                  val aboveHasDoc = hasProperScaladocAbove(i)
                  val nearbyHasDoc = hasProperScaladocAbove(i, 24)
                  val alreadyTodo = if start > 0 then cleanedLines(start - 1).contains("TODO FILL IN") else false
                  val lastOpen = (0 until i).reverse.find(k => cleanedLines(k).contains("/*")).getOrElse(-1)
                  val lastClose = (0 until i).reverse.find(k => cleanedLines(k).contains("*/")).getOrElse(-1)
                  val inOrAfterBlock = lastOpen > lastClose
                  val docLikeBetween = (start until i).exists { k =>
                    val t = cleanedLines(k).trim
                    t.startsWith("*") || t.startsWith("/**") || t.startsWith("/*") ||
                    t.contains("@param") || t.contains("@return") || t.contains("@see")
                  }

                  if !betweenHasDoc && !aboveHasDoc && !nearbyHasDoc && !alreadyTodo && !inOrAfterBlock && !docLikeBetween then
                    val indent = leadingWhitespace(line)
                    acc :+ (start -> s"${indent}/** TODO FILL IN */")
                  else acc
                else acc
            case None => acc
        else acc
      }

    if edits.isEmpty then None
    else
      val insertMap = edits.groupBy(_._1).view.mapValues(_.map(_._2).toList).toMap
      val withInserts = cleanedLines.zipWithIndex.flatMap { case (ln, idx) =>
        insertMap.getOrElse(idx, Nil) ++ List(ln)
      }
      val tailInserts = insertMap.getOrElse(cleanedLines.length, Nil)
      Some((path, (withInserts ++ tailInserts).toSeq))

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
