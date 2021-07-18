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

  val yourTree: String = "AAA YXY"
  var huffmanTest1 = new HuffmanLib(yourTree)
  val encoded = huffmanTest1.myEncode(yourTree.toVector)
  val decoded = huffmanTest1.myDecode(encoded)

  test("This is a test"){
    assertEquals(decoded, yourTree.toVector)
  }
}