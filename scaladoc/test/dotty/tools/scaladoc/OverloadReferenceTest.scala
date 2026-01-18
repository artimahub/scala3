package dotty.tools.scaladoc

import org.junit.Test

class OverloadReferenceTest extends BaseHtmlTest:

  @Test
  def testOverloadReferences(): Unit = withGeneratedDoc(Seq("tests")) {
    withHtmlFile("tests/OverloadReferenceTest.html") { ctx =>
      // Check that the buffer reference links to the correct overload
      ctx.assertAttr(
        ".documentableBrief a",
        "href",
        "scala/jdk/javaapi/CollectionConverters.html#asJava[A](b:scala.collection.mutable.Buffer[A])*"
      )

      // Check that the map reference links to the correct overload
      ctx.assertAttr(
        ".documentableBrief a",
        "href",
        "scala/jdk/javaapi/CollectionConverters.html#asJava[K,V](m:scala.collection.Map[K,V])*"
      )

      // Check that the seq reference links to the correct overload
      ctx.assertAttr(
        ".documentableBrief a",
        "href",
        "scala/jdk/javaapi/CollectionConverters.html#asJava[A](s:scala.collection.Seq[A])*"
      )

      // Ensure there are no unresolved links
      ctx.assertNotExists(unresolvedLinkSelector)
    }
  }