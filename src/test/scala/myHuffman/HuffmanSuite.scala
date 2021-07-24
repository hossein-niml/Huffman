package myHuffman

class HuffmanSuite extends munit.FunSuite {

  val yourTree: String = "AAABBC"

  val huffmanTest1: Huffman = Huffman(yourTree)

  val testInput: String = (0 to 66 map(_ => "A")).mkString
  val myTest: String = "ACAAB"

  val encoded: Huffman.EncodeResult = huffmanTest1.encode(myTest)

  val decoded: String = huffmanTest1.decode(encoded)

  test("This is a test"){
    assertEquals(decoded, myTest)
  }
}