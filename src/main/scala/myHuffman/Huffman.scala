package myHuffman

import java.util.InputMismatchException
import scala.annotation.tailrec

class Huffman(myStr: String) {

  import Huffman._

  private val thisTree: CodeTree = createCodeTree(myStr.toVector)

  //public methods
  def getTree: CodeTree = thisTree

  def encode(str: String): EncodeResult = {
    processEncoder(str.toVector, pointer = 1, Vector(0), reminder = 0, firstTime = true)
  }

  def decode(encoded: EncodeResult): String = {
    decoder(thisTree, encoded.encoded, encoded.bitsCount).mkString
  }

  //private methods
  @tailrec
  private def mergeTwoInt(n: Int, bc: Int, pointer: Int, current: Vector[Int]): Vector[Int] = {
    if (bc == 0) {
      current
    } else {
      if (pointer > INT_MAX_INDEX) {
        mergeTwoInt(n, bc, pointer = 1, current :+ 0)
      } else {
        val msb = getMSB(n, bc)
        val newNum = appendToRight(current.last, msb)
        mergeTwoInt(n & ((1 << (bc - 1)) - 1), bc - 1, pointer + 1, current.dropRight(1) :+ newNum)
      }
    }
  }

  @tailrec
  private def processEncoder(text: Vector[Char], pointer: Int, current: Vector[Int], reminder: Int, firstTime: Boolean): EncodeResult = {
    if (text.isEmpty) {
      EncodeResult(current, reminder)
    } else {
      val textBitCountRem = bitCount(thisTree, text) % INT_MAX_INDEX
      val countRem = if (textBitCountRem != 0) textBitCountRem else INT_MAX_INDEX
      val nextReminder = if (firstTime) countRem else reminder
      val head = text.head
      val bc = bitCount(thisTree, Vector(head))
      val currentEncoded = mergeTwoInt(encoder(thisTree)(Vector(head)), bc, pointer, current)
      if (text.tail.isEmpty) {
        EncodeResult(currentEncoded, nextReminder)
      } else {
        val newPointer = if ((bc + pointer) % INT_MAX_INDEX != 0) (bc + pointer) % INT_MAX_INDEX else 32
        val nextCurrent = if (newPointer == 1) currentEncoded :+ 0 else currentEncoded
        processEncoder(text.tail, newPointer, nextCurrent, nextReminder, firstTime = false)
      }
    }
  }

  private def appendToRight(num: Int, bit: Int): Int = {
    (num << 1) + bit
  }

  private def getMSB(num: Int, bitCount: Int): Int = {
    val shifted = 1 << (bitCount - 1)
    if ((num & shifted) == shifted) 1 else 0
  }

  private def weight(tree: CodeTree): Int = {
    tree match {
      case CodeTree.Leaf(_, w1) => w1
      case CodeTree.Fork(left, right, _, _) => weight(left) + weight(right)
    }
  }

  private def chars(tree: CodeTree): Vector[Char] = {
    tree match {
      case CodeTree.Leaf(c, _) => Vector(c)
      case CodeTree.Fork(left, right, _, _) => chars(left) ++ chars(right)
    }
  }

  private def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree = {
    CodeTree.Fork(left, right, chars(left) ++ chars(right), weight(left) + weight(right))
  }

  private def times(chars: Vector[Char]): Map[Char, Int] = {
    if (chars.isEmpty) {
      Map.empty
    } else {
      val ch = chars.head
      val rest = chars.tail
      val tail = times(rest)
      tail.filter(r => r._1 != ch) + (ch -> (findTimes(ch, tail) + 1))
    }
  }

  private def findTimes(c: Char, v: Map[Char, Int]): Int = {
    v.getOrElse(c, 0)
  }

  private def makeOrderedLeafList(freqs: Map[Char, Int]): Vector[CodeTree.Leaf] = {
    if (freqs.isEmpty) {
      Vector.empty
    } else {
      freqs.map(p => CodeTree.Leaf(p._1, p._2)).toVector.sortWith((r1, r2) => weight(r1) < weight(r2))
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

  private def decoder(tree: CodeTree, bits: Vector[Int], bitsCount: Int): Vector[Char] = {
    if (bits.isEmpty) {
      Vector.empty
    } else {
      if (bits.size == 1) {
        decoderHelper(tree, tree, bits, bitsCount, bitsCount)
      } else {
        decoderHelper(tree, tree, bits, bitsCount, INT_MAX_INDEX)
      }
    }
  }


  private def decoderHelper(mainTree: CodeTree, tree: CodeTree, bits: Vector[Int], bitsCount: Int, bc: Int): Vector[Char] = {
    if (bc == 0) {
      tree match {
        case CodeTree.Leaf(char, _) =>
          if (bits.size > 1) {
            if (bits.tail.size == 1) {
              char +: decoderHelper(mainTree, mainTree, bits.tail, bitsCount, bitsCount)
            } else {
              char +: decoderHelper(mainTree, mainTree, bits.tail, bitsCount, INT_MAX_INDEX)
            }
          } else {
            Vector(char)
          }

        case CodeTree.Fork(_, _, _, _) =>
          if (bits.size > 1) {
            if (bits.tail.size == 1) {
              decoderHelper(mainTree, tree, bits.tail, bitsCount, bitsCount)
            } else {
              decoderHelper(mainTree, tree, bits.tail, bitsCount, INT_MAX_INDEX)
            }
          } else {
            throw new InputMismatchException()
          }
      }
    } else {
      tree match {
        case CodeTree.Leaf(char, _) =>
          char +: decoderHelper(mainTree, mainTree, bits, bitsCount, bc)

        case CodeTree.Fork(left, right, _, _) =>
          val shiftTest = 1 << (bc - 1)
          if ((shiftTest & bits.head) != shiftTest) {
            decoderHelper(mainTree, left, bits, bitsCount, bc - 1)
          } else if ((shiftTest & bits.head) == shiftTest) {
            decoderHelper(mainTree, right, bits, bitsCount, bc - 1)
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
      text.map(ch => encoderOneChar(tree)(ch).size).sum
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
      case CodeTree.Fork(left, right, _, _) =>
        left match {
          case CodeTree.Fork(_, _, l_chars, _) =>
            if (l_chars.contains(c)) {
              0.toByte +: encoderOneChar(left)(c)
            } else {
              1.toByte +: encoderOneChar(right)(c)
            }

          case CodeTree.Leaf(l_char, _) =>
            if (l_char.equals(c)) {
              0.toByte +: encoderOneChar(left)(c)
            } else {
              1.toByte +: encoderOneChar(right)(c)
            }
        }

      case CodeTree.Leaf(_, _) => Vector.empty
    }
  }

}

object Huffman {

  def apply(str: String): Huffman = {
    new Huffman(str)
  }

  type Bit = Byte

  private final val INT_MAX_INDEX: Int = 32

  sealed abstract class CodeTree

  object CodeTree {

    case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree

    case class Leaf(char: Char, weight: Int) extends CodeTree

  }

  case class EncodeResult(encoded: Vector[Int], bitsCount: Int)

}