package myHuffman

class HuffmanSuite extends munit.FunSuite {

  def tester(tree: String, testInput: String): Unit = {
    val huffmanTest: Huffman = Huffman(tree)
    val encoded: Huffman.EncodeResult = huffmanTest.encode(testInput)
    val decoded: String = huffmanTest.decode(encoded)
    val name = "Tree: '" ++ tree ++ "'  Input: '" ++ testInput ++ "'"
    test(name) {
      assertEquals(decoded, testInput)
    }
  }

  tester(tree = "this is A easy Test !", testInput = "!A !!Test is a eA test")
  tester(tree = "your Tests Are very EasY :D", testInput = "eT you are TesteD")
  tester(tree = "lets get Tests harder and more HARDER www", testInput = "That was A HARD TEst and tested me hardddd")
  tester(tree = "I think This program works so good", testInput = "IwTh tnkgdooo oogImmppkk kkkkkk")
  tester(tree = "bbbbrrrdu lets check the final test and we most pass it", testInput = "final test is the most important test and should be passed")
  tester(tree = "i want to push it on github", testInput = "push github push github push github push github push github")
}