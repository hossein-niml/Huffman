package myHuffman
import Huffman._

class HuffmanLib(val myStr: String) {

  private var thisTree: CodeTree = createCodeTree(myStr.toVector)

  def setTree(newStr: String) {
    thisTree = createCodeTree(newStr.toVector)
  }

  def getTree(): CodeTree = {
    thisTree
  }

  def decode(bits: Vector[Bit]): String = {
    Huffman.decode(thisTree, bits).mkString
  }

  def encode(str: String): Vector[Bit] = {
    Huffman.encode(thisTree)(str.toVector)
  }

}