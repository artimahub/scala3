package todowriter

import java.util.regex.Pattern

/** Documentation kind for external mappings. */
enum DocumentationKind:
  case Javadoc extends DocumentationKind
  case Scaladoc2 extends DocumentationKind
  case Scaladoc3 extends DocumentationKind

object DocumentationKind:
  def fromString(s: String): Option[DocumentationKind] =
    s.toLowerCase match
      case "javadoc" => Some(Javadoc)
      case "scaladoc2" => Some(Scaladoc2)
      case "scaladoc3" => Some(Scaladoc3)
      case _ => None

/** Represents an external documentation mapping.
 *
 *  @param originRegexes Regular expressions matching classpath entries
 *  @param documentationUrl Base URL of external documentation
 *  @param kind Type of documentation (Javadoc, Scaladoc2, Scaladoc3)
 *  @param packageListUrl Optional URL to package-list file
 */
case class ExternalDocLink(
  originRegexes: List[Pattern],
  documentationUrl: String,
  kind: DocumentationKind,
  packageListUrl: Option[String] = None
)

object ExternalDocLink:
  /** Parse external mapping from string format.
   *
   *  Format: `regex::docTool::url[::packageList]`
   *
   *  Examples:
   *  - `.*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/`
   *  - `.*scala/.*::scaladoc3::https://www.scala-lang.org/api/current/`
   *
   *  @param mapping String in the format `regex::docTool::url[::packageList]`
   *  @return Parsed ExternalDocLink or error message
   */
  def parse(mapping: String): Either[String, ExternalDocLink] =
    val parts = mapping.split("::").toList
    if parts.length < 3 then
      Left(s"Invalid mapping format: $mapping (expected: regex::docTool::url[::packageList])")
    else
      val regex :: docTool :: url :: packageListOpt = parts: @unchecked
      try
        val pattern = Pattern.compile(regex)
        DocumentationKind.fromString(docTool) match
          case Some(kind) =>
            Right(ExternalDocLink(
              originRegexes = List(pattern),
              documentationUrl = url,
              kind = kind,
              packageListUrl = packageListOpt.headOption
            ))
          case None =>
            Left(s"Unknown documentation kind: $docTool (expected: javadoc, scaladoc2, scaladoc3)")
      catch
        case e: Exception => Left(s"Invalid regex in mapping: $mapping (${e.getMessage})")

  /** Parse multiple external mappings from a file.
   *
   *  Each line should contain one mapping in the format `regex::docTool::url[::packageList]`
   *  Lines starting with `#` are treated as comments.
   *
   *  @param content File content with one mapping per line
   *  @return List of parsed ExternalDocLink or list of errors
   */
  def parseFile(content: String): Either[List[String], List[ExternalDocLink]] =
    val lines = content.linesIterator
      .map(_.trim)
      .filterNot(_.isEmpty)
      .filterNot(_.startsWith("#"))
      .toList

    val results = lines.map(parse)
    val errors = results.collect { case Left(err) => err }
    val mappings = results.collect { case Right(mapping) => mapping }

    if errors.nonEmpty then Left(errors)
    else Right(mappings)

  /** Check if a class name matches any of the external mapping patterns.
   *
   *  @param className Fully qualified class name (e.g., "java.util.List")
   *  @param mappings List of external mappings to check
   *  @return Matching ExternalDocLink or None
   */
  def findMapping(className: String, mappings: List[ExternalDocLink]): Option[ExternalDocLink] =
    mappings.find(mapping =>
      mapping.originRegexes.exists(_.matcher(className).matches())
    )

  /** Construct the documentation URL for a given class using the external mapping.
   *
   *  @param className Fully qualified class name (e.g., "java.util.List")
   *  @param member Optional member name (e.g., "add" for "java.util.List.add")
   *  @param mapping External mapping to use for URL construction
   *  @return Full documentation URL
   */
  def constructUrl(className: String, member: Option[String], mapping: ExternalDocLink): String =
    mapping.kind match
      case DocumentationKind.Javadoc =>
        // Javadoc URLs: https://docs.oracle.com/javase/8/docs/api/java/util/List.html
        // With anchor for methods: https://docs.oracle.com/javase/8/docs/api/java/util/List.html#add-java.lang.Object-
        val path = className.replace('.', '/')
        val base = s"${mapping.documentationUrl}$path.html"
        member match
          case Some(memberName) =>
            // For methods, construct a simple anchor (simplified version)
            // Full parameter matching would require more complex parsing
            s"$base#$memberName"
          case None => base

      case DocumentationKind.Scaladoc2 =>
        // Scaladoc2 URLs: https://www.scala-lang.org/api/2.13.12/scala/collection/immutable/List.html
        val path = className.replace('.', '/')
        val base = s"${mapping.documentationUrl}$path.html"
        member match
          case Some(memberName) => s"$base#$memberName"
          case None => base

      case DocumentationKind.Scaladoc3 =>
        // Scaladoc3 URLs: https://www.scala-lang.org/api/current/scala/collection/immutable/List
        val path = className.replace('.', '/')
        val base = s"${mapping.documentationUrl}$path"
        member match
          case Some(memberName) => s"$base#$memberName"
          case None => base