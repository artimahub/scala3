package todowriter

import java.nio.file.{Files, Path, Paths}

object Main:
  case class Config(
      folder: Option[Path] = None,
      fix: Boolean = false,
      json: Boolean = false,
      help: Boolean = false,
      skipTodo: Boolean = false
  )

  def main(args: Array[String]): Unit =
    val config = parseArgs(args.toList)

    if config.help then
      printHelp()
      System.exit(0)

    config.folder match
      case None =>
        System.err.println("Error: folder argument required")
        printHelp()
        System.exit(2)

      case Some(folder) =>
        if !Files.isDirectory(folder) then
          System.err.println(s"Error: folder not found: $folder")
          System.exit(2)
 
        run(folder, config.fix, config.json, config.skipTodo)

  private def parseArgs(args: List[String]): Config =
    args match
      case Nil => Config()
      case "--help" :: rest => parseArgs(rest).copy(help = true)
      case "--dry" :: rest => parseArgs(rest).copy(fix = true)
      case "--json" :: rest => parseArgs(rest).copy(json = true)
      case "--skip-todo" :: rest => parseArgs(rest).copy(skipTodo = true)
      case arg :: rest if arg.startsWith("-") =>
        System.err.println(s"Unknown option: $arg")
        parseArgs(rest)
      case folder :: rest =>
        parseArgs(rest).copy(folder = Some(Paths.get(folder)))

  private def printHelp(): Unit =
    println("""todo-writer - Scaladoc tag checker and fixer
              |
              |Usage: todo-writer <folder> [options]
              |
              |Arguments:
              |  folder              Root folder to scan for .scala files
              |
              |Options:
              |  --dry               Dry run (do not write changes to files)
              |  --skip-todo         Do not insert TODO placeholders for missing tags when fixing
              |  --json              Output results as JSON
              |  --help              Show this help message
              |
              |Exit codes:
              |  0                   No issues found (or --fix applied successfully)
              |  1                   Issues found (dry run)
              |  2                   Error (folder not found, etc.)
              |""".stripMargin)

  private def run(folder: Path, dry: Boolean, json: Boolean, skipTodo: Boolean): Unit =
    val results = ScaladocChecker.checkDirectory(folder)
    val summary = ScaladocChecker.summarize(results)
 
    if json then
      println(formatJson(results, summary))
    else
      print(ScaladocChecker.formatReport(results, summary))
 
    if !dry then
      // insertTodo should be the inverse of skipTodo: when skipTodo is true, do not insert TODOs
      applyFixes(results, insertTodo = !skipTodo)
 
    // Exit code
    if dry && summary.totalIssues > 0 then
      System.exit(1)
    else
      System.exit(0)

  private def applyFixes(results: List[FileResult], insertTodo: Boolean): Unit =
    for fileResult <- results if fileResult.hasIssues do
      val path = Paths.get(fileResult.path)
      val fixResult = Fixer.fixFile(path, fileResult.results, insertTodo)
      fixResult.newContent match
        case Some(content) =>
          Fixer.writeFixedFile(path, content)
          println(s"Applied fixes to ${fixResult.path} (${fixResult.blocksFixed} blocks)")
        case None =>
          ()

  private def formatJson(results: List[FileResult], summary: Summary): String =
    val sb = new StringBuilder
    sb.append("{\n")
    sb.append("  \"files\": [\n")

    val fileEntries = results.map { fileResult =>
      val resultsJson = fileResult.results.map { checkResult =>
        val issuesJson = checkResult.issues.map(i => s"\"${escapeJson(i.message)}\"").mkString("[", ", ", "]")
        s"""    {
           |      "line": ${checkResult.scaladoc.lineNumber},
           |      "kind": "${checkResult.declaration.kind}",
           |      "name": "${escapeJson(checkResult.declaration.name)}",
           |      "issues": $issuesJson
           |    }""".stripMargin
      }.mkString(",\n")

      s"""  {
         |    "path": "${escapeJson(fileResult.path)}",
         |    "results": [
         |$resultsJson
         |    ]
         |  }""".stripMargin
    }

    sb.append(fileEntries.mkString(",\n"))
    sb.append("\n  ],\n")
    sb.append(s"""  "summary": {
                 |    "totalFiles": ${summary.totalFiles},
                 |    "totalIssues": ${summary.totalIssues},
                 |    "methods": { "total": ${summary.defTotal}, "withIssues": ${summary.defWithIssues} },
                 |    "classes": { "total": ${summary.classTotal}, "withIssues": ${summary.classWithIssues} },
                 |    "traits": { "total": ${summary.traitTotal}, "withIssues": ${summary.traitWithIssues} }
                 |  }
                 |}""".stripMargin)

    sb.toString

  private def escapeJson(s: String): String =
    s.replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
