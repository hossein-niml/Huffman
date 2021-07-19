package myHuffman

import java.util.InputMismatchException
import scala.annotation.tailrec

class HuffmanLib(myStr: String) {

  import HuffmanLib._

  case class encodeResult(encoded: Vector[Int], bitsCount: Vector[Int])

  private val thisTree: CodeTree = Huffman.createCodeTree(myStr.toVector)

  def getTree: CodeTree = thisTree

  def decode(encoded: encodeResult): String = {
    Huffman.decode(thisTree, encoded.encoded, encoded.bitsCount).mkString
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
      findBestSliceIndex(tree, revVec.tail.reverse, size, i + 1)
    }
  }

  def encode(str: String): encodeResult = {
    val myVec = str.toVector
    val myBits = Huffman.bitCount(thisTree, myVec)
    if (myBits <= INT_MAX_INDEX) {
      encodeResult(Vector(Huffman.encode(thisTree)(myVec)), Vector(Huffman.bitCount(thisTree, myVec)))
    } else {
      val sliceIndex = findBestSliceIndex(thisTree, myVec, myVec.size, 0)
      val vecToEncode = myVec.take(sliceIndex)
      val encoded = Huffman.encode(thisTree)(vecToEncode)
      val tailEncode = encode(myVec.drop(sliceIndex).mkString)
      encodeResult(encoded +: tailEncode.encoded, Huffman.bitCount(thisTree, vecToEncode) +: tailEncode.bitsCount)
    }
  }
}

object HuffmanLib {
  val INT_MAX_INDEX = 32

  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  trait Huffman {

    type Bit = Byte

    def weight(tree: CodeTree): Int = {
      tree match {
        case Leaf(_, w1) => w1
        case Fork(left, right, _, _) => weight(left) + weight(right)
      }
    }

    def chars(tree: CodeTree): Vector[Char] = {
      tree match {
        case Leaf(c, _) => Vector(c)
        case Fork(left, right, _, _) => chars(left) ++ chars(right)
      }
    }

    def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree =
      Fork(left, right, chars(left) ++ chars(right), weight(left) + weight(right))

    def times(chars: Vector[Char]): Vector[(Char, Int)] = {
      if (chars.isEmpty) {
        Vector.empty
      } else {
        val ch = chars.head
        val rest = chars.tail
        val tail = times(rest)
        tail.filter(r => r._1 != ch) :+ ((ch, findTimes(ch, tail) + 1))
      }
    }

    def findTimes(c: Char, v: Vector[(Char, Int)]): Int = {
      val y = v.find(r => r._1 == c)
      if (y.isEmpty) {
        0
      } else {
        y.head._2
      }
    }

    def makeOrderedLeafList(freqs: Vector[(Char, Int)]): Vector[Leaf] = {
      if (freqs.isEmpty) {
        Vector.empty
      } else {
        freqs.map(p => Leaf(p._1, p._2)).sortWith((r1, r2) => weight(r1) < weight(r2))
      }
    }

    def singleton(trees: Vector[CodeTree]): Boolean = {
      if (trees.isEmpty) {
        false
      } else {
        trees.tail.isEmpty
      }
    }

    def combine(trees: Vector[CodeTree]): Vector[CodeTree] = {
      if (trees.isEmpty || singleton(trees)) {
        trees
      } else {
        (makeCodeTree(trees.head, trees.tail.head) +: trees.tail.tail).sortWith((x, y) => weight(x) < weight(y))
      }
    }

    def until(done: Vector[CodeTree] => Boolean, merge: Vector[CodeTree] => Vector[CodeTree])(trees: Vector[CodeTree]): Vector[CodeTree] = {
      if (done(trees)) {
        trees
      } else {
        until(done, merge)(merge(trees))
      }
    }

    def createCodeTree(chars: Vector[Char]): CodeTree = {
      until(singleton, combine)(makeOrderedLeafList(times(chars))).head
    }

    def decode(tree: CodeTree, bits: Vector[Int], bitsCount: Vector[Int]): Vector[Char] = {
      if (bits.isEmpty) {
        Vector.empty
      } else {
        decodeHelper(tree, tree, bits.head, bitsCount.head) ++ decode(tree, bits.tail, bitsCount.tail)
      }
    }

    def bitsOfInt(inp: Int, i: Int): Int = {
      if ((1 << i) > inp) {
        i - 1
      } else {
        bitsOfInt(inp, i + 1)
      }
    }

    def decodeHelper(mainTree: CodeTree, tree: CodeTree, bits: Int, bc: Int): Vector[Char] = {
      if (bc < 0) {
        Vector.empty
      } else {
        tree match {
          case Leaf(char, _) =>
            char +: decodeHelper(mainTree, mainTree, bits, bc)

          case Fork(left, right, _, _) =>
            val shiftTest = 1 << (bc - 1)
            if ((shiftTest & bits) != shiftTest) {
              decodeHelper(mainTree, left, bits, bc - 1)
            } else if ((shiftTest & bits) == shiftTest) {
              decodeHelper(mainTree, right, bits, bc - 1)
            } else {
              throw new InputMismatchException()
            }
        }
      }
    }

    def bitCount(tree: CodeTree, text: Vector[Char]): Int = {
      if (text.isEmpty) {
        0
      } else {
        val head = text.head
        val tail = text.tail
        encodeOneChar(tree)(head).size + bitCount(tree, tail)
      }
    }

    def shift(rev: Vector[Bit], i: Int): Int = {
      if (rev.isEmpty) {
        0
      } else {
        val head = rev.head
        val tail = rev.tail
        (head << i) + shift(tail, i + 1)
      }
    }

    def valueOfOneChar(tree: CodeTree, char: Char, bitsOfRest: Int): Int = {
      val myVec = encodeOneChar(tree)(char)
      shift(myVec.reverse, bitsOfRest)
    }

    def encode(tree: CodeTree)(text: Vector[Char]): Int = {
      if (text.isEmpty) {
        0
      } else {
        val x = text.head
        val y = text.tail
        valueOfOneChar(tree, x, bitCount(tree, y)) + encode(tree)(y)
      }
    }

    def encodeOneChar(tree: CodeTree)(c: Char): Vector[Bit] = {
      tree match {
        case Fork(left, right, _, _) =>
          left match {
            case Fork(_, _, l_chars, _) =>
              if (l_chars.contains(c)) {
                0.toByte +: encodeOneChar(left)(c)
              } else {
                1.toByte +: encodeOneChar(right)(c)
              }

            case Leaf(l_char, _) =>
              if (l_char.equals(c)) {
                0.toByte +: encodeOneChar(left)(c)
              } else {
                1.toByte +: encodeOneChar(right)(c)
              }
          }

        case Leaf(_, _) => Vector.empty
      }
    }
  }

  object Huffman extends Huffman

  def apply(str: String): HuffmanLib = {
    new HuffmanLib(str)
  }
}