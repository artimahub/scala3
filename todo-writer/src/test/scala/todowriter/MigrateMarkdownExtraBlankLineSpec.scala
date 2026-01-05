package todowriter

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.Files
import java.util.regex.{Pattern, Matcher}

class MigrateMarkdownExtraBlankLineSpec extends AnyFlatSpec with Matchers:

  "migrateScaladocInner replacement" should "not insert additional star-only blank lines when migrating scaladoc" in {
    val tmp = Files.createTempDirectory("migrate-test")
    try
      val file = tmp.resolve("Boolean.scala")
      val original =
        """package example
          |
          |/**
          | *
          | * Compares two Boolean expressions ''italic'' and returns `true` if they evaluate to a different value.
          | *
          | */
          |object X
          |""".stripMargin
      Files.writeString(file, original)

      // Simulate performMigration for the single file by applying WikidocToMarkdown.migrateScaladocInner
      val text = Files.readString(file)
      val pattern = Pattern.compile("(?s)/\\*\\*(.*?)\\*/")
      val m = pattern.matcher(text)
      val sb = new StringBuffer
      var any = false
      while m.find() do
        val inner = m.group(1)
        val migrated = WikidocToMarkdown.migrateScaladocInner(inner)
        if migrated != inner then
          any = true
          val replacement = "/**" + migrated + "*/"
          m.appendReplacement(sb, java.util.regex.Matcher.quoteReplacement(replacement))
      m.appendTail(sb)
      if any then Files.writeString(file, sb.toString)

      val content = Files.readString(file)
      // There must NOT be an extra blank line between two star-only lines introduced by migration.
      val doubleBlank = Pattern.compile("(?m)^\\s*\\*\\s*$\\r?\\n\\s*\\r?\\n\\s*\\*\\s*")
      doubleBlank.matcher(content).find() should be (false)
    finally
      // cleanup
      def deleteRec(p: java.nio.file.Path): Unit =
        if Files.isDirectory(p) then
          val it = Files.list(p).iterator()
          while it.hasNext do deleteRec(it.next())
          Files.delete(p)
        else Files.delete(p)
      deleteRec(tmp)
  }