package myHuffman

import java.util
import java.util.InputMismatchException

/**
 * A huffman code is represented by a binary tree.
 *
 * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
 * The weight of a `Leaf` is the frequency of appearance of the character.
 *
 * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
 * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
 * leaves.
 */
abstract class CodeTree

case class Fork(left: CodeTree, right: CodeTree, chars: Vector[Char], weight: Int) extends CodeTree

case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = {
    tree match {
      case Leaf(char, w1) => w1
      case Fork(left, right, chars, w2) => weight(left) + weight(right)
    }
  }

  def chars(tree: CodeTree): Vector[Char] = {
    tree match {
      case Leaf(c, w1) => Vector(c)
      case Fork(left, right, cx, w2) => chars(left) ++ chars(right)
    }
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ++ chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with Lists of characters. This function allows
   * you to easily create a character List from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the List `chars` the number of
   * times it occurs. For example, the invocation
   *
   * times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting List is not important):
   *
   * List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a List of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   * val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   * val theChar = pair._1
   * val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   * pair match {
   * case (theChar, theInt) =>
   * println("character is: "+ theChar)
   * println("integer is  : "+ theInt)
   * }
   */
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
    var y = v.find(r => r._1 == c)
    if (y.isEmpty) {
      0
    } else {
      y.head._2
    }
  }

  /**
   * Returns a List of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned List should be ordered by ascending weights (i.e. the
   * head of the List should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: Vector[(Char, Int)]): Vector[Leaf] = {
    if (freqs.isEmpty) {
      Vector.empty
    } else {
      //insert(freqs.head, makeOrderedLeafList(freqs.tail))
      freqs.map(p => Leaf(p._1, p._2)).sortWith((r1, r2) => weight(r1) < weight(r2))
    }
  }

  def insert(x: (Char, Int), y: Vector[Leaf]): Vector[Leaf] = {
    if (y.isEmpty) {
      Vector(Leaf(x._1, x._2))
    } else {
      if (x._2 < weight(y.head)) {
        Leaf(x._1, x._2) +: y
      } else {
        y.head +: insert(x, y.tail)
      }
    }
  }

  /**
   * Checks whether the List `trees` contains only one single code tree.
   */
  def singleton(trees: Vector[CodeTree]): Boolean = {
    if (trees.isEmpty) { false } else { trees.tail.isEmpty }
  }

  /**
   * The parameter `trees` of this function is a List of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the List `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a List of less than two elements, that List should be returned
   * unchanged.
   */
  def combine(trees: Vector[CodeTree]): Vector[CodeTree] = {
    if (trees.isEmpty || singleton(trees)) {
      trees
    } else {
//      var z = makeCodeTree(trees.head, trees.tail.head)
//      newInsert(z, trees.filter(y => weight(y) > weight(trees.tail.head)))
      (makeCodeTree(trees.head, trees.tail.head) +: trees.tail.tail).sortWith((x, y) => weight(x) < weight(y))
    }
  }

  def newInsert(ct: CodeTree, l: Vector[CodeTree]): Vector[CodeTree] = {
    if (l.isEmpty) {
      Vector(ct)
    } else {
      if (weight(ct) < weight(l.head)) {
        ct +: l
      } else {
        l.head +: newInsert(ct, l.tail)
      }
    }
  }


  /**
   * This function will be called in the following way:
   *
   * until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the List of
   * code trees contains only one single tree, and then return that singleton List.
   */
  def until(done: Vector[CodeTree] => Boolean, merge: Vector[CodeTree] => Vector[CodeTree])(trees: Vector[CodeTree]): Vector[CodeTree] = {
    if (done(trees)) {
      trees
    } else {
      until(done, merge)(merge(trees))
    }
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: Vector[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }


  // Part 3: Decoding

  type Bit = Byte

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting List of characters.
   */
  def decode(tree: CodeTree, bits: Vector[Bit]): Vector[Char] = {
    decodeHelper(tree, tree, bits)
  }

  def decodeHelper(mainTree: CodeTree, tree: CodeTree, bits: Vector[Bit]): Vector[Char] = {
    tree match {

      case Leaf(char, weight) =>
        if (bits.isEmpty) {
          Vector(char)
        } else {
          char +: decodeHelper(mainTree, mainTree, bits)
        }

      case Fork(left, right, chars, weight) =>
        if (bits.head == 0.toByte) {
          decodeHelper(mainTree, left, bits.tail)
        } else if (bits.head == 1.toByte) {
          decodeHelper(mainTree, right, bits.tail)
        } else {
          throw new InputMismatchException()
        }
    }
  }
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), Vector('x', 'j'), 14279), Leaf('f', 16351), Vector('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), Vector('k', 'w'), 2492), Vector('z', 'k', 'w'), 4585), Leaf('y', 4725), Vector('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), Vector('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), Vector('z', 'k', 'w', 'y', 'h', 'q'), 41497), Vector('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), Vector('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), Vector('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), Vector('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), Vector('m', 'p'), 91856), Leaf('u', 96785), Vector('m', 'p', 'u'), 188641), Vector('o', 'l', 'm', 'p', 'u'), 355071), Vector('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), Vector('g', 'b'), 27110), Vector('v', 'g', 'b'), 52085), Vector('c', 'v', 'g', 'b'), 102088), Vector('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), Vector('n', 't'), 219915), Vector('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), Vector('i', 'a'), 232575), Vector('e', 'i', 'a'), 458522), Vector('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), Vector('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: Vector[Bit] = Vector(0.toByte, 0.toByte, 1.toByte, 1.toByte, 1.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte, 1.toByte, 1.toByte, 0.toByte, 0.toByte, 1.toByte, 1.toByte, 0.toByte, 1.toByte, 0.toByte, 0.toByte, 1.toByte, 1.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte, 1.toByte, 0.toByte, 0.toByte, 1.toByte, 1.toByte, 1.toByte, 1.toByte, 1.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte, 1.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte, 1.toByte, 1.toByte, 0.toByte, 0.toByte, 1.toByte, 0.toByte, 0.toByte, 1.toByte, 0.toByte, 0.toByte, 0.toByte, 1.toByte, 0.toByte, 0.toByte, 0.toByte, 1.toByte, 0.toByte, 1.toByte)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: Vector[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
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
      case Fork(left, right, chars, weight1) =>
        left match {
          case Fork(l_left, l_right, l_chars, l_weight1) =>
            if (l_chars.contains(c)){
              0.toByte +: encodeOneChar(left)(c)
            } else {
              1.toByte +: encodeOneChar(right)(c)
            }

          case Leaf(l_char, l_weight2) =>
            if (l_char.equals(c)) {
              0.toByte +: encodeOneChar(left)(c)
            } else {
              1.toByte +: encodeOneChar(right)(c)
            }
        }

      case Leaf(char, weight2) => Vector.empty
    }
    
  }

  // Part 4b: Encoding using code table

  type CodeTable = Vector[(Char, Vector[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): Vector[Bit] = {
    if (table.isEmpty) {
      Vector.empty
    } else {
      val head = table.head
      if (head._1 == char){
        head._2
      } else {
        codeBits(table.tail)(char)
      }
    }
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    tree match {
      case Fork(left, right, chars, weight) =>
        mergeCodeTables(convert(left), convert(right))
      case Leaf(char, weight) =>
        Vector((char, encodeOneChar(tree)(char)))
    }
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    a ++ b
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: Vector[Char]): Vector[Bit] = {
    if (text.isEmpty) {
      Vector.empty
    } else {
      val x = text.head
      val y = text.tail
      codeBits(convert(tree))(x) ++ quickEncode(tree)(y)
    }
  }

}
object Huffman extends Huffman
