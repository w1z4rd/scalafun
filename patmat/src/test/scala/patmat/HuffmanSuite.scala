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
    val t3 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5),Fork(Leaf('d',4), Leaf('a',2), List('a', 'd'), 6), List('a','b','d'), 11)
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

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('y', 6))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), Leaf('y', 6)))
  }
   
  test("until of some leaf list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('y', 6))
    assert(until(singleton, combine)(leaflist) === List(Fork(Leaf('y', 6), Fork(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), List('x', 'e', 't'), 9), List('y', 'x', 'e', 't'), 15)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("times a char occurs in a char list") {
    assert(times(string2Chars("hello world")) === List(('d',1), ('l',3), ('r',1), ('o',2), ('w',1), (' ',1), ('e',1), ('h',1)))
    assert(times(Nil) === Nil)
    }

  test("singleton check if the list of trees contains only one code tree") {
    new TestTrees {
      assert(singleton(List(t2)))
      assert(!singleton(List(t1,t2)))
    }
  }

  test("createCodeTree of some string") {
    assert(createCodeTree(string2Chars("hello world")) === Fork(Fork(Fork(Leaf('e', 1), Leaf('h', 1), List('e', 'h'), 2), Fork(Leaf('w', 1), Leaf(' ', 1), List('w',' ' ), 2), List('e', 'h', 'w', ' '), 4), Fork(Leaf('l', 3), Fork(Fork(Leaf('d', 1), Leaf('r', 1), List('d', 'r'), 2), Leaf('o', 2), List('d', 'r', 'o'), 4), List('l', 'd', 'r', 'o'), 7), List('e', 'h', 'w', ' ', 'l', 'd', 'r', 'o'), 11))
  }
  
  test("decode short text") {
    new TestTrees {
      val secret: List[Bit] = List(1,0)
     assert(decode(t1, secret)=== List('b', 'a')) 
    }
  }
  
  test("encode short text") {
    new TestTrees {
      assert(encode(t1)(string2Chars("ba")) === List(1,0))
    }
  }
  
  test("codeBits returns the correct list of bits") {
    val codeTable: CodeTable = List(('a', List[Bit](0,1,1)),('b', List[Bit](1,0,1)))
    assert(codeBits(codeTable)('b') === List[Bit](1,0,1))
  }

  test("mergeCodeTables returns correct CodeTable") {
    assert(mergeCodeTables(Nil, Nil) === Nil)
    assert(mergeCodeTables(Nil, List[(Char, List[Bit])](('a', List[Bit](0)),('b', List[Bit](1)))) ===  List[(Char, List[Bit])](('a', List[Bit](0)),('b', List[Bit](1))))
    assert(mergeCodeTables(List[(Char, List[Bit])](('a', List[Bit](0)),('b', List[Bit](1))), Nil) ===  List[(Char, List[Bit])](('a', List[Bit](0)),('b', List[Bit](1))))
    assert(mergeCodeTables(List[(Char, List[Bit])](('a', List[Bit](0)),('b', List[Bit](1))), List[(Char, List[Bit])](('c', List[Bit](0)), ('d', List[Bit](1)))) ===  List[(Char, List[Bit])](('a', List[Bit](0,0)),('b', List[Bit](0,1)), ('c', List[Bit](1,0)), ('d', List[Bit](1,1))))
  }

  test("convert CodeTree returns correct CodeTable"){
    new TestTrees {
    assert(convert(t2) === List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
    }
  }
  
  test("quickEncode works quickly"){
    new TestTrees {
      assert(quickEncode(t2)(string2Chars("abbadabba")) === List[Bit](0,0,0,1,0,1,0,0,1,0,0,0,1,0,1,0,0))
    }
    println(decodedSecret)
  }
}
