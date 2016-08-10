package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(Nil) === Nil)
    val cl = List('a', 'b', 'a', 'b', 'a')
    assert(times(cl).length === 2, "Length must be 2")
    def elementFilter(ec: Char, ei: Int) = (pair: (Char, Int)) => pair._1 == ec && pair._2 == ei
    assert(times(cl).exists(elementFilter('a', 3)))
    assert(times(cl).exists(elementFilter('b', 2)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(!singleton(leaflist))
    assert(!singleton(leaflist.drop(1)))
    assert(singleton(leaflist.drop(2)))
    assert(!singleton(Nil))

    val ll = List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))
    assert(!singleton(ll))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val ll1 = combine(leaflist)
    val ll2 = combine(ll1)
    val ll3 = combine(ll2)
    assert(ll1 === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    assert(ll2 === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7)))
    assert(ll3 === ll2, ll3)
  }

  test("until") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val r = until(singleton, combine)(leaflist)
    val e = List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
    assert(r.size === 1, "until must keep on working until there is only one CodeTree" + r)
    assert(r === e)
  }

  test("decode") {
    val ct = Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)
    val tettex = List(0,1,0,0,0,1,0,1,0,0,1)
    val d = List('t','e','t','t','e','x')
    assert(decode(ct, tettex) === d)
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
