package myHuffman

import Huffman._

class HuffmanLib(val myStr: String) {

  private val thisTree: CodeTree = createCodeTree(myStr.toVector)

  def getTree(): CodeTree = thisTree

  def decode(bits: Vector[Bit]): String = Huffman.decode(thisTree, bits).mkString

  def encode(str: String): Vector[Bit] = Huffman.encode(thisTree)(str.toVector)

}

object HuffmanLib {
  def apply(str: String): HuffmanLib ={
    new HuffmanLib(str)
  }
}