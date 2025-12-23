package todowriter

object DebugAsterisk2:
  def main(args: Array[String]): Unit =
    val text = """    /**
                 |     * Testing
                 |     */
                 |    def bar(x: Int): Int = ???""".stripMargin
    val block = ScaladocBlock.findAll(text).head
    val result = Fixer.buildFixedBlock(text, block, Nil, Nil, false)
    println("----- ORIGINAL -----")
    println(text)
    println("----- RESULT -----")
    println(result)
    println("----- RAW LINES -----")
    result.split("\n").zipWithIndex.foreach { (l,i) => println(s"$i: '${l}'") }