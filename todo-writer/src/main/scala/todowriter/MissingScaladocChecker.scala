package todowriter

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

object MissingScaladocChecker:
  /** Insert /** TODO FILL IN */ scaladoc blocks for public/protected defs/classes/traits/objects with no scaladoc.
   *
   *  Respects `dry`: when dry is true, only reports what would be changed.
   */
  def insertMissingScaladocTodos(folder: Path, dry: Boolean): Unit =
    import java.nio.file.{Files => JFiles, Paths => JPaths}
    import scala.jdk.CollectionConverters._
    val scalaFiles = JFiles.walk(folder).filter(p => p.toString.endsWith(".scala")).iterator().asScala.toList

    // Line-oriented declaration matcher (handles common modifiers/annotations)
    val DeclLinePattern = """^\s*(?:@[\w\(\)\s,."']+\s*)*(?:private|protected|final|override|inline|implicit|given|export|opaque|sealed|abstract|lazy|case\s+)*\s*(class|trait|object|def)\b.*""".r

    val modifierKeywords = Set(
      "private", "protected", "final", "override", "inline", "implicit",
      "given", "export", "opaque", "sealed", "abstract", "lazy", "case"
    )

    var totalInserted = 0

    for path <- scalaFiles do
      val text = Files.readString(path)
      // Preserve trailing empty line by using split with -1
      val origLines = text.split("\n", -1).toBuffer

      // Precompute scaladoc block ranges (start,end) in the original file so
      // detection of attached scaladoc is robust even in large files.
      import scala.collection.mutable.ListBuffer
      val scaladocRanges = {
        val rb = ListBuffer.empty[(Int, Int)]
        var s = 0
        while s < origLines.length do
          // Detect scaladoc start/end anywhere on the line (handles mid-line "/**" like "}/** ...")
          if origLines(s).indexOf("/**") >= 0 then
            var e = s
            // walk to the end of the comment (inclusive). If not found, treat to EOF.
            while e < origLines.length && origLines(e).indexOf("*/") < 0 do e += 1
            rb += ((s, e))
            s = e + 1
          else s += 1
        rb.toList
      }

      // Helper: check range (e) is the most recent scaladoc before decl index i
      def nearestScaladocBefore(declIdx: Int): Option[(Int, Int)] =
        // scaladocRanges sorted by end ascending by construction; find the one with max end < declIdx
        val before = scaladocRanges.filter { case (_, e) => e < declIdx }
        if before.isEmpty then None else Some(before.maxBy(_._2))
      
      // Helper: determine whether this declaration is nested inside a 'private' type (class/trait/object)
      def enclosingTypeIsPrivate(declIdx: Int): Boolean =
        var j = declIdx - 1
        while j >= 0 do
          DeclLinePattern.findFirstMatchIn(origLines(j)) match
            case Some(mm) =>
              val knd = mm.group(1)
              if List("class", "trait", "object").contains(knd) then
                if mm.matched.contains("private") then return true
                else return false
            case None => ()
          j -= 1
        false

      // Helper: lines between (from..to) are blank or modifier-only/annotations
      def onlyModifiersOrBlank(from: Int, to: Int): Boolean =
        var j = from
        while j <= to do
          val s = origLines(j).trim
          if s.nonEmpty then
            val firstTok = s.split("\\s+").headOption.getOrElse("")
            if !(firstTok.startsWith("@") || modifierKeywords.contains(firstTok)) then return false
          j += 1
        true

      val out = collection.mutable.ArrayBuffer[String]()
      var fileInserted = 0

      var i = 0
      while i < origLines.length do
        val line = origLines(i)
        DeclLinePattern.findFirstMatchIn(line) match
          case None =>
            out += line
          case Some(m) =>
            val matched = m.matched
            val kind = m.group(1)
            // Only target these kinds and skip private
            if !List("class", "trait", "object", "def").contains(kind) || matched.contains("private") then
              out += line
            else
              // If the declaration line itself is inside an existing scaladoc block,
              // skip to avoid inserting inside comments (protects against malformed files).
              val declInsideScaladoc =
                scaladocRanges.exists { case (s, e) => s <= i && i <= e }

              if declInsideScaladoc then
                out += line
              else
                // Walk back from declaration, skipping blank lines and modifier-only lines,
                // to locate the nearest "real" previous line.
                var k = i - 1
                while k >= 0 && origLines(k).trim.isEmpty do k -= 1
                var continue = true
                while k >= 0 && continue do
                  val t = origLines(k).trim
                  val firstTok = if t.isEmpty then "" else t.split("\\s+")(0)
                  if firstTok.startsWith("@") || modifierKeywords.contains(firstTok) then k -= 1
                  else continue = false
                // Now k is the index of nearest non-modifier/annotation/non-blank line (or -1)

                val attached = nearestScaladocBefore(i) match
                  case None => false
                  case Some((s, e)) =>
                    // require that that scaladoc is the most recent non-comment content
                    // before the declaration: all lines between e+1 and i-1 must be blank
                    // or modifier/annotation-only. This is stricter and avoids attaching a
                    // scaladoc that belongs to a different nearby declaration.
                    val betweenFrom = e + 1
                    val betweenTo = i - 1
                    if betweenFrom <= betweenTo then onlyModifiersOrBlank(betweenFrom, betweenTo)
                    else true

                if attached then
                  out += line
                else
                  // Skip insertion if the declaration is inside a private enclosing type
                  if enclosingTypeIsPrivate(i) then
                    out += line
                  // Avoid inserting when a scaladoc starts mid-line (e.g. "}/** ...")
                  else if origLines(i).indexOf("/**") >= 0 && !origLines(i).trim.startsWith("/**") then
                    out += line
                  else
                    // Insert TODO marker with same leading indentation as declaration
                    val leadingWs = line.takeWhile(_.isWhitespace)
                    out += s"${leadingWs}/** TODO FILL IN */"
                    out += line
                    fileInserted += 1
        i += 1

      if fileInserted > 0 then
        val newText = out.mkString("\n")
        if dry then
          println(s"[dry] Would insert $fileInserted scaladoc TODO(s) into: $path")
        else
          Files.writeString(path, newText)
          println(s"Inserted $fileInserted scaladoc TODO(s) into: $path")
        totalInserted += fileInserted

    if totalInserted == 0 then println("No missing scaladoc found.") else println(s"Inserted $totalInserted scaladoc TODO(s).")