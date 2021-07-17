package myHuffman


class HuffmanSuite extends munit.FunSuite {

  def testMe(huffmanLib: HuffmanLib, input: Vector[Int]): Unit = {
    val myInput = input.map(v => v.toByte)
    val seq = myInput.mkString("-")
    val name = "Encode and Decode " ++ seq
    test(name){
      val decoded = huffmanLib.myDecode(myInput)
      val encoded = huffmanLib.myEncode(decoded)
      assertEquals(encoded, myInput)
    }
  }

  val yourTree: String = "AAAAABBBC"
  var huffmanTest1 = new HuffmanLib(yourTree)

  testMe(huffmanTest1, Vector(0, 1))
  testMe(huffmanTest1, Vector(1, 1, 1))
  testMe(huffmanTest1, Vector(0, 1, 0, 0, 1))
  testMe(huffmanTest1, Vector(0, 0, 1, 0, 0))
}