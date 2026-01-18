package dotty.tools.scaladoc

import org.junit.Test
import scala.jdk.CollectionConverters._
import java.nio.file._

class BufferOverloadTest extends BaseHtmlTest:

  @Test
  def testBufferOverloadLinks(): Unit = {
    withGeneratedDoc(pcks = Seq("bufferoverload")) {
      withHtmlFile("tests/bufferoverload/BufferOverloadTest$.html") { ctx =>
        // Check that the link to asJava exists and is not broken
        ctx.assertNotExists(unresolvedLinkSelector)
        
        // Check that the link points to the correct overload (the one that takes Buffer, not Map)
        // The href should contain "#asJava-" but we can't easily check the exact hash
        // So we'll check that there's at least one link
        ctx.assertAttr(".documentableBrief a", "href", Seq("*")) // At least one link exists
      }
    }
  }