package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(List('a', 'b', 'a', 'c', 'd', 'a', 'b', 'a'))") {
    assert(times(List('a', 'b', 'a', 'c', 'd', 'a', 'b', 'a')) === List(('a', 4), ('b', 2), ('c', 1), ('d', 1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton") {
    assert(singleton(List()) === false, "nil is not single")
    assert(singleton(List(Leaf('c', '1'))) === true, "leaf is single")
    assert(singleton(List(Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), '2'))) === true, "fork is single")
    assert(singleton(List(Leaf('a', 1), Leaf('b', 1))) === false, "list of 2 leaves is not single")
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val leaflist2 = List(Leaf('a', 3), Leaf('b', 2), Leaf('c', 1))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    assert(combine(leaflist2) === List(Leaf('c', 1), Fork(Leaf('a', 3), Leaf('b', 2), List('a', 'b'), 5)), "reorder")
  }

  test("until singleton") {
    val trees = List(Leaf('a', 1), Leaf('b', 1))
    assert(until(singleton, combine)(trees) == List(Fork(Leaf('a', 1),Leaf('b',1),List('a', 'b'),2)))
  }

  test("createCodeTree") {
    val charList = List('a', 'b', 'a')
    assert(createCodeTree(charList) == Fork(Leaf('b',1), Leaf('a', 2), List('b', 'a'),3))
  }

  test("decode short text") {
    new TestTrees {
      assert(decode(t2, List(0, 0, 0, 1, 1)) == List('a', 'b', 'd'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits") {
    val table: CodeTable = List(('a', List(0, 0, 0)), ('b', List(0, 0, 1)), ('c', List(0, 1, 0)))
    assert(codeBits(table)('c') === List(0, 1, 0))
  }

}
