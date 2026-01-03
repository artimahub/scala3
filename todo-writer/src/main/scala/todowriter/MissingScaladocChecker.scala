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
              // Scan backwards in 'out' to find the previous meaningful line
              var j = out.length - 1
              // Skip blank lines
              while j >= 0 && out(j).trim.isEmpty do j -= 1
              // Skip annotation or modifier-only lines (they may appear between scaladoc and decl)
              def isModifierOnly(s: String): Boolean =
                val t = s.trim
                if t.isEmpty then false
                else
                  val tok = t.split("\\s+")(0)
                  modifierKeywords.contains(tok) || t.startsWith("@")
              var scan = j
              while scan >= 0 && isModifierOnly(out(scan)) do scan -= 1

              val hasAttachedScaladoc =
                if scan >= 0 then
                  // If the nearest non-modifier/annotation previous line ends with */ we assume a scaladoc is attached
                  out(scan).trim.endsWith("*/")
                else false

              if hasAttachedScaladoc then
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