package myHuffman

import java.util.InputMismatchException
import scala.annotation.tailrec

class Huffman(myStr: String) {

  import Huffman._

  private val thisTree: CodeTree = createCodeTree(myStr.toVector)

  def getTree: CodeTree = thisTree

  def encode(str: String): EncodeResult = {
    val myVec = str.toVector
    val myBits = bitCount(thisTree, myVec)
    if (myBits <= INT_MAX_INDEX) {
      EncodeResult(Vector(encoder(thisTree)(myVec)), Vector(bitCount(thisTree, myVec)))
    } else {
      val sliceIndex = findBestSliceIndex(thisTree, myVec, myVec.size, 0)
      val vecToEncode = myVec.take(sliceIndex)
      val encoded = encoder(thisTree)(vecToEncode)
      val tailEncode = encode(myVec.drop(sliceIndex).mkString)
      EncodeResult(encoded +: tailEncode.encoded, bitCount(thisTree, vecToEncode) +: tailEncode.bitsCount)
    }
  }

  def decode(encoded: EncodeResult): String = {
    decoder(thisTree, encoded.encoded, encoded.bitsCount).mkString
  }

  private def weight(tree: CodeTree): Int = {
    tree match {
      case Leaf(_, w1) => w1
      case Fork(left, right, _, _) => weight(left) + weight(right)
    }
  }

  private def chars(tree: CodeTree): Vector[Char] = {
    tree match {
      case Leaf(c, _) => Vector(c)
      case Fork(left, right, _, _) => chars(left) ++ chars(right)
    }
  }

  private def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree = {
    Fork(left, right, chars(left) ++ chars(right), weight(left) + weight(right))
  }

  private def times(chars: Vector[Char]): Vector[(Char, Int)] = {
    if (chars.isEmpty) {
      Vector.empty
    } else {
      val ch = chars.head
      val rest = chars.tail
      val tail = times(rest)
      tail.filter(r => r._1 != ch) :+ ((ch, findTimes(ch, tail) + 1))
    }
  }

  private def findTimes(c: Char, v: Vector[(Char, Int)]): Int = {
    val y = v.find(r => r._1 == c)
    if (y.isEmpty) {
      0
    } else {
      y.head._2
    }
  }

  private def makeOrderedLeafList(freqs: Vector[(Char, Int)]): Vector[Leaf] = {
    if (freqs.isEmpty) {
      Vector.empty
    } else {
      freqs.map(p => Leaf(p._1, p._2)).sortWith((r1, r2) => weight(r1) < weight(r2))
    }
  }

  private def singleton(trees: Vector[CodeTree]): Boolean = {
    if (trees.isEmpty) {
      false
    } else {
      trees.tail.isEmpty
    }
  }

  private def combine(trees: Vector[CodeTree]): Vector[CodeTree] = {
    if (trees.isEmpty || singleton(trees)) {
      trees
    } else {
      (makeCodeTree(trees.head, trees.tail.head) +: trees.tail.tail).sortWith((x, y) => weight(x) < weight(y))
    }
  }

  @tailrec
  private def until(done: Vector[CodeTree] => Boolean, merge: Vector[CodeTree] => Vector[CodeTree])(trees: Vector[CodeTree]): Vector[CodeTree] = {
    if (done(trees)) {
      trees
    } else {
      until(done, merge)(merge(trees))
    }
  }

  private def createCodeTree(chars: Vector[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }

  private def decoder(tree: CodeTree, bits: Vector[Int], bitsCount: Vector[Int]): Vector[Char] = {
    if (bits.isEmpty) {
      Vector.empty
    } else {
      decoderHelper(tree, tree, bits.head, bitsCount.head) ++ decoder(tree, bits.tail, bitsCount.tail)
    }
  }


  private def decoderHelper(mainTree: CodeTree, tree: CodeTree, bits: Int, bc: Int): Vector[Char] = {
    if (bc < 0) {
      Vector.empty
    } else {
      tree match {
        case Leaf(char, _) =>
          char +: decoderHelper(mainTree, mainTree, bits, bc)

        case Fork(left, right, _, _) =>
          val shiftTest = 1 << (bc - 1)
          if ((shiftTest & bits) != shiftTest) {
            decoderHelper(mainTree, left, bits, bc - 1)
          } else if ((shiftTest & bits) == shiftTest) {
            decoderHelper(mainTree, right, bits, bc - 1)
          } else {
            throw new InputMismatchException()
          }
      }
    }
  }

  private def bitCount(tree: CodeTree, text: Vector[Char]): Int = {
    if (text.isEmpty) {
      0
    } else {
      val head = text.head
      val tail = text.tail
      encoderOneChar(tree)(head).size + bitCount(tree, tail)
    }
  }

  private def shift(rev: Vector[Bit], i: Int): Int = {
    if (rev.isEmpty) {
      0
    } else {
      val head = rev.head
      val tail = rev.tail
      (head << i) + shift(tail, i + 1)
    }
  }

  private def valueOfOneChar(tree: CodeTree, char: Char, bitsOfRest: Int): Int = {
    val myVec = encoderOneChar(tree)(char)
    shift(myVec.reverse, bitsOfRest)
  }

  private def encoder(tree: CodeTree)(text: Vector[Char]): Int = {
    if (text.isEmpty) {
      0
    } else {
      val x = text.head
      val y = text.tail
      valueOfOneChar(tree, x, bitCount(tree, y)) + encoder(tree)(y)
    }
  }

  private def encoderOneChar(tree: CodeTree)(c: Char): Vector[Bit] = {
    tree match {
      case Fork(left, right, _, _) =>
        left match {
          case Fork(_, _, l_chars, _) =>
            if (l_chars.contains(c)) {
              0.toByte +: encoderOneChar(left)(c)
            } else {
              1.toByte +: encoderOneChar(right)(c)
            }

          case Leaf(l_char, _) =>
            if (l_char.equals(c)) {
              0.toByte +: encoderOneChar(left)(c)
            } else {
              1.toByte +: encoderOneChar(right)(c)
            }
        }

      case Leaf(_, _) => Vector.empty
    }
  }

  @tailrec
  private def findBestSliceIndex(tree: CodeTree, vec: Vector[Char], size: Int, i: Int): Int = {
    val numOfBits = bitCount(tree, vec)
    val revVec = vec.reverse
    val head = revVec.head
    val headBits = bitCount(tree, Vector(head))
    if (numOfBits - headBits <= INT_MAX_INDEX) {
      size - i - 1
    } else {
      findBestSliceIndex(tree, revVec.tail.reverse, size, i + 1)
    }
  }

}

object Huffman {

  def apply(str: String): Huffman = {
    new Huffman(str)
  }

  type Bit = Byte

  val INT_MAX_INDEX = 32

  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  case class EncodeResult(encoded: Vector[Int], bitsCount: Vector[Int])
}