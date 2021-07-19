package myHuffman

trait HuffmanInterface {
  def weight(tree: CodeTree): Int

  def chars(tree: CodeTree): Vector[Char]

  def times(chars: Vector[Char]): Vector[(Char, Int)]

  def makeOrderedLeafList(freqs: Vector[(Char, Int)]): Vector[Leaf]

  def singleton(trees: Vector[CodeTree]): Boolean

  def combine(trees: Vector[CodeTree]): Vector[CodeTree]

  def until(done: Vector[CodeTree] => Boolean, merge: Vector[CodeTree] => Vector[CodeTree])(trees: Vector[CodeTree]): Vector[CodeTree]

  def createCodeTree(chars: Vector[Char]): CodeTree

  def decode(tree: CodeTree, bits: Vector[Int], bitsCount: Vector[Int]): Vector[Char]

  def encode(tree: CodeTree)(text: Vector[Char]): Int
}
