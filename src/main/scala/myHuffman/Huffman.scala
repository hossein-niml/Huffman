package myHuffman

import java.util.InputMismatchException

abstract class CodeTree

case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree

trait Huffman extends HuffmanInterface {

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
    if (trees.isEmpty) { false } else { trees.tail.isEmpty }
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

  type Bit = Byte

  def decode(tree: CodeTree, bits: Vector[Bit]): Vector[Char] = {
    decodeHelper(tree, tree, bits)
  }

  def decodeHelper(mainTree: CodeTree, tree: CodeTree, bits: Vector[Bit]): Vector[Char] = {
    tree match {

      case Leaf(char, _) =>
        if (bits.isEmpty) {
          Vector(char)
        } else {
          char +: decodeHelper(mainTree, mainTree, bits)
        }

      case Fork(left, right, _, _) =>
        if (bits.head == 0.toByte) {
          decodeHelper(mainTree, left, bits.tail)
        } else if (bits.head == 1.toByte) {
          decodeHelper(mainTree, right, bits.tail)
        } else {
          throw new InputMismatchException()
        }
    }
  }

  def encode(tree: CodeTree)(text: Vector[Char]): Vector[Bit] = {
    if (text.isEmpty) {
      Vector.empty
    } else {
      val x = text.head
      val y = text.tail
      encodeOneChar(tree)(x) ++ encode(tree)(y)
    }
  }
  
  def encodeOneChar(tree: CodeTree)(c: Char) : Vector[Bit] = {
    tree match {
      case Fork(left, right, _, _) =>
        left match {
          case Fork(_, _, l_chars, _) =>
            if (l_chars.contains(c)){
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
