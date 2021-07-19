package myHuffman

import Huffman._

import scala.annotation.tailrec

class HuffmanLib(val myStr: String) {

  import HuffmanLib.INT_MAX_INDEX

  private val thisTree: CodeTree = createCodeTree(myStr.toVector)

  var encodedBits: Vector[Int] = Vector.empty

  def getTree: CodeTree = thisTree

  def decode(encoded: Vector[Int]): String = {
    Huffman.decode(thisTree, encoded, encodedBits).mkString
  }

  @tailrec
  private def findBestSliceIndex(tree: CodeTree, vec: Vector[Char], size: Int, i: Int): Int = {
    val numOfBits = Huffman.bitCount(tree, vec)
    val revVec = vec.reverse
    val head = revVec.head
    val headBits = Huffman.bitCount(tree, Vector(head))
    if (numOfBits - headBits <= INT_MAX_INDEX) {
      size - i - 1
    } else {
      findBestSliceIndex(tree, revVec.tail.reverse, size, i+1)
    }
  }

  def encode(str: String): Vector[Int] = {
    val myVec = str.toVector
    val myBits = Huffman.bitCount(thisTree, myVec)
    if (myBits <= INT_MAX_INDEX) {
      encodedBits = encodedBits :+ Huffman.bitCount(thisTree, myVec)
      Vector(Huffman.encode(thisTree)(myVec))
    } else {
      val sliceIndex = findBestSliceIndex(thisTree, myVec, myVec.size, 0)
      val vecToEncode = myVec.take(sliceIndex)
      val encoded = Huffman.encode(thisTree)(vecToEncode)
      encodedBits = encodedBits :+ Huffman.bitCount(thisTree, vecToEncode)
      encoded +: encode(myVec.drop(sliceIndex).mkString)
    }
  }

}

object HuffmanLib {
  val INT_MAX_INDEX = 32
  def apply(str: String): HuffmanLib = {
    new HuffmanLib(str)
  }
}