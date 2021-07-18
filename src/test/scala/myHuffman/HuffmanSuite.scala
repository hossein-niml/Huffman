package myHuffman

class HuffmanSuite extends munit.FunSuite {

  val yourTree: String = "AAA YXY"
  val huffmanTest1 = HuffmanLib(yourTree)

  val testInput: String = "AXAX "
  val encoded = huffmanTest1.encode(testInput)
  val decoded = huffmanTest1.decode(encoded)

  test("This is a test"){
    assertEquals(decoded, testInput)
  }

}