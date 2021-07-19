package myHuffman

class HuffmanSuite extends munit.FunSuite {

  val yourTree: String = "AAABBC"
  val huffmanTest1: HuffmanLib = HuffmanLib(yourTree)

  val testInput: String = (0 to 50 map(_ => "A")).mkString
  val myTest: String = "ACAAB"

  val encoded: huffmanTest1.encodeResult = huffmanTest1.encode(myTest)

  val decoded: String = huffmanTest1.decode(encoded)

  test("This is a test"){
    assertEquals(decoded, myTest)
  }
}