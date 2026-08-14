/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package util

import scala.language.`2.13`
import java.io.{IOException, PrintWriter}
import java.util.jar.Attributes.{Name => AttributeName}
import scala.annotation.tailrec
import language.experimental.captureChecking

/** Loads `library.properties` from the jar. */
object Properties extends PropertiesTrait {
  /** TODO FILL IN */
  protected def propCategory = "library"
  /** TODO FILL IN */
  protected def pickJarBasedOn: Class[Option[?]] = classOf[Option[?]]

  /** Scala manifest attributes.
   */
  val ScalaCompilerVersion = new AttributeName("Scala-Compiler-Version")
}

private[scala] trait PropertiesTrait {
  /** TODO FILL IN */
  protected def propCategory: String      // specializes the remainder of the values
  /** TODO FILL IN */
  protected def pickJarBasedOn: Class[?]  // props file comes from jar containing this

  /** The name of the properties file. */
  protected val propFilename = "/" + propCategory + ".properties"

  /** The loaded properties. */
  protected lazy val scalaProps: java.util.Properties = {
    val props = new java.util.Properties
    val stream = pickJarBasedOn.getResourceAsStream(propFilename)
    if (stream ne null)
      quietlyDispose(props.load(stream), stream.close)

    props
  }

  private def quietlyDispose(action: => Unit, disposal: => Unit) =
    try     { action }
    finally {
        try     { disposal }
        catch   { case _: IOException => }
    }

  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def propIsSet(name: String): Boolean                   = System.getProperty(name) != null
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @param value TODO FILL IN
   */
  def propIsSetTo(name: String, value: String)           = propOrNull(name) == value
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def propOrNone(name: String): Option[String]           = Option[String](System.getProperty(name))
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @param alt TODO FILL IN
   *  @return TODO FILL IN
   */
  def propOrElse(name: String, alt: => String): String   = propOrNone(name).getOrElse(alt)
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def propOrEmpty(name: String): String                  = propOrElse(name, "")
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def propOrNull(name: String): String | Null            = propOrNone(name).orNull
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def propOrFalse(name: String): Boolean                 = propOrNone(name) exists (x => List("yes", "on", "true") contains x.toLowerCase)
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @param value TODO FILL IN
   *  @return TODO FILL IN
   */
  def setProp(name: String, value: String): String       = System.setProperty(name, value)
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def clearProp(name: String): String                    = System.clearProperty(name)

  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @param alt TODO FILL IN
   *  @return TODO FILL IN
   */
  def envOrElse(name: String, alt: => String): String    = Option(System.getenv(name)) getOrElse alt
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def envOrNone(name: String): Option[String]            = Option(System.getenv(name))

  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @param alt TODO FILL IN
   */
  def envOrSome(name: String, alt: => Option[String])    = envOrNone(name) orElse alt

  // for values based on propFilename, falling back to System properties
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @param alt TODO FILL IN
   *  @return TODO FILL IN
   */
  def scalaPropOrElse(name: String, alt: => String): String = scalaPropOrNone(name).getOrElse(alt)
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def scalaPropOrEmpty(name: String): String             = scalaPropOrElse(name, "")
  /** TODO FILL IN
   *
   *  @param name TODO FILL IN
   *  @return TODO FILL IN
   */
  def scalaPropOrNone(name: String): Option[String]      = Option(scalaProps.getProperty(name)).orElse(propOrNone("scala." + name))

  /** The version of the Scala runtime, if this is not a snapshot.
   */
  val releaseVersion = scalaPropOrNone("maven.version.number").filterNot(_.endsWith("-SNAPSHOT"))

  /** The version of the Scala runtime, if this is a snapshot.
   */
  val developmentVersion = scalaPropOrNone("maven.version.number").filter(_.endsWith("-SNAPSHOT")).flatMap(_ => scalaPropOrNone("version.number"))

  /** The version of the Scala runtime, or the empty string if unknown.
   *
   *  Note that the version of the Scala library need not correlate with the version of the Scala compiler
   *  used to emit either the library or user code.
   *
   *  For example, Scala 3.0 and 3.1 use the Scala 2.13 library, which is reflected in this version string.
   *  For the Dotty version, see `dotty.tools.dotc.config.Properties.versionNumberString`.
   */
  def versionNumberString = scalaPropOrEmpty("version.number")

  /** A verbose alternative to [[versionNumberString]].
   */
  val versionString         = s"version ${scalaPropOrElse("version.number", "(unknown)")}"
  /** TODO FILL IN */
  val copyrightString       = scalaPropOrElse("copyright.string", "Copyright 2002-2025, LAMP/EPFL and Lightbend, Inc. dba Akka")

  /** This is the encoding to use reading in source files, overridden with -encoding.
   *  Note that it uses "prop" i.e. looks in the scala jar, not the system properties.
   */
  def sourceEncoding        = scalaPropOrElse("file.encoding", "UTF-8")
  /** TODO FILL IN */
  def sourceReader          = scalaPropOrElse("source.reader", "scala.tools.nsc.io.SourceReader")

  /** This is the default text encoding, overridden (unreliably) with
   *  `JAVA_OPTS="-Dfile.encoding=Foo"`
   */
  def encodingString        = propOrElse("file.encoding", "UTF-8")

  /** The default end of line character.
   */
  def lineSeparator: String = System.lineSeparator()

  /* Various well-known properties. */
  /** TODO FILL IN */
  def javaClassPath         = propOrEmpty("java.class.path")
  /** TODO FILL IN */
  def javaHome              = propOrEmpty("java.home")
  /** TODO FILL IN */
  def javaVendor            = propOrEmpty("java.vendor")
  /** TODO FILL IN */
  def javaVersion           = propOrEmpty("java.version")
  /** TODO FILL IN */
  def javaVmInfo            = propOrEmpty("java.vm.info")
  /** TODO FILL IN */
  def javaVmName            = propOrEmpty("java.vm.name")
  /** TODO FILL IN */
  def javaVmVendor          = propOrEmpty("java.vm.vendor")
  /** TODO FILL IN */
  def javaVmVersion         = propOrEmpty("java.vm.version")
  /** TODO FILL IN */
  def javaSpecVersion       = propOrEmpty("java.specification.version")
  /** TODO FILL IN */
  def javaSpecVendor        = propOrEmpty("java.specification.vendor")
  /** TODO FILL IN */
  def javaSpecName          = propOrEmpty("java.specification.name")
  /** TODO FILL IN */
  def osName                = propOrEmpty("os.name")
  /** TODO FILL IN */
  def scalaHome             = propOrEmpty("scala.home")
  /** TODO FILL IN */
  def tmpDir                = propOrEmpty("java.io.tmpdir")
  /** TODO FILL IN */
  def userDir               = propOrEmpty("user.dir")
  /** TODO FILL IN */
  def userHome              = propOrEmpty("user.home")
  /** TODO FILL IN */
  def userName              = propOrEmpty("user.name")

  /* Some derived values. */
  /** Returns `true` iff the underlying operating system is a version of Microsoft Windows. */
  lazy val isWin            = osName.startsWith("Windows")
  // See https://mail.openjdk.java.net/pipermail/macosx-port-dev/2012-November/005148.html for
  // the reason why we don't follow developer.apple.com/library/mac/#technotes/tn2002/tn2110.
  /** Returns `true` iff the underlying operating system is a version of Apple Mac OSX. */
  lazy val isMac            = osName.startsWith("Mac OS X")
  /** Returns `true` iff the underlying operating system is a Linux distribution. */
  lazy val isLinux          = osName.startsWith("Linux")

  /* Some runtime values. */
  private[scala] lazy val isAvian = javaVmName.contains("Avian")

  private[scala] def coloredOutputEnabled: Boolean = propOrElse("scala.color", "auto") match {
    case "auto" => consoleIsTerminal
    case s      => "" == s || "true".equalsIgnoreCase(s)
  }

  /** System.console.isTerminal, or just check for null console on JDK < 22. */
  private[scala] lazy val consoleIsTerminal: Boolean = {
    import scala.reflect.Selectable.reflectiveSelectable
    val console = System.console
    def isTerminal: Boolean =
      try console.asInstanceOf[{ def isTerminal(): Boolean }].isTerminal()
      catch { case _: NoSuchMethodException => false }
    console != null && (!isJavaAtLeast("22") || isTerminal)
  }

  // This is looking for javac, tools.jar, etc.
  // Tries JDK_HOME first, then the more common but likely jre JAVA_HOME,
  // and finally the system property based javaHome.
  /** TODO FILL IN */
  def jdkHome               = envOrElse("JDK_HOME", envOrElse("JAVA_HOME", javaHome))

  private[scala] def versionFor(command: String) = s"Scala $command $versionString -- $copyrightString"

  /** TODO FILL IN */
  def versionMsg            = versionFor(propCategory)
  /** TODO FILL IN */
  def scalaCmd              = if (isWin) "scala.bat" else "scala"
  /** TODO FILL IN */
  def scalacCmd             = if (isWin) "scalac.bat" else "scalac"

  /** Compares the given specification version to the specification version of the platform.
   *
   *  @param version a specification version number (legacy forms acceptable)
   *  @return `true` if the specification version of the current runtime
   *    is equal to or higher than the version denoted by the given string.
   *  @throws NumberFormatException if the given string is not a version string
   *
   *  @example ```
   *  // In this example, the runtime's Java specification is assumed to be at version 8.
   *  isJavaAtLeast("1.8")            // true
   *  isJavaAtLeast("8")              // true
   *  isJavaAtLeast("9")              // false
   *  isJavaAtLeast("9.1")            // false
   *  isJavaAtLeast("1.9")            // throws
   *  ```
   */
  def isJavaAtLeast(version: String): Boolean = {
    def versionOf(s: String, depth: Int): (Int, String) =
      s.indexOf('.') match {
        case 0 =>
          (-2, s.substring(1))
        case 1 if depth == 0 && s.charAt(0) == '1' =>
          val r0 = s.substring(2)
          val (v, r) = versionOf(r0, 1)
          val n = if (v > 8 || r0.isEmpty) -2 else v   // accept 1.8, not 1.9 or 1.
          (n, r)
        case -1 =>
          val n = if (!s.isEmpty) s.toInt else if (depth == 0) -2 else 0
          (n, "")
        case i  =>
          val r = s.substring(i + 1)
          val n = if (depth < 2 && r.isEmpty) -2 else s.substring(0, i).toInt
          (n, r)
      }
    @tailrec
    def compareVersions(s: String, v: String, depth: Int): Int = {
      if (depth >= 3) 0
      else {
        val (sn, srest) = versionOf(s, depth)
        val (vn, vrest) = versionOf(v, depth)
        if (vn < 0) -2
        else if (sn < vn) -1
        else if (sn > vn) 1
        else compareVersions(srest, vrest, depth + 1)
      }
    }
    compareVersions(javaSpecVersion, version, 0) match {
      case -2 => throw new NumberFormatException(s"Not a version: $version")
      case i  => i >= 0
    }
  }

  /** Compares the given specification version to the major version of the platform.
   *
   *  @param version a specification major version number
   *  @return `true` if the specification version of the current runtime is equal to or higher than the given version
   */
  def isJavaAtLeast(version: Int): Boolean = isJavaAtLeast(math.max(version, 0).toString)

  // provide a main method so version info can be obtained by running this
  /** TODO FILL IN
   *
   *  @param args TODO FILL IN
   */
  def main(args: Array[String]): Unit = {
    val writer = new PrintWriter(Console.err, true)
    writer.println(versionMsg)
  }
}
