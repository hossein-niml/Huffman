package myHuffman
import Huffman._

class HuffmanLib(val myStr: String) {

  private var thisTree: CodeTree = createCodeTree(myStr.toVector)

  def setTree(newStr: String) {
    thisTree = createCodeTree(newStr.toVector)
  }

  def myDecode(bits: Vector[Bit]): Vector[Char] = {
    decode(thisTree, bits)
  }

  def myEncode(str: Vector[Char]): Vector[Bit] = {
    encode(thisTree)(str)
  }

}

//object HuffmanLib {
//  def apply(str: String): HuffmanLib ={
//    new HuffmanLib(createCodeTree(str.toVector))
//  }
//}