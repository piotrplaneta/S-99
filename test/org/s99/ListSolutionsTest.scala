import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.s99.ListSolutions

import ListSolutions._

@RunWith(classOf[JUnitRunner])
class ListSolutionsTest extends FunSuite {
  test("last - normal list") {
    assert(ListSolutions.last(List(1, 2, 3)) === Some(3))
  }

  test("last - empty list") {
    assert(ListSolutions.last(List()) === None)
  }

  test("last but one - normal list") {
    assert(ListSolutions.lastButOne(List(1, 2, 3, 4)) === Some(3))
  }

  test("last but one - list with one element") {
    assert(ListSolutions.lastButOne(List(1)) === None)
  }

  test("last but one - empty list") {
    assert(ListSolutions.lastButOne(List()) === None)
  }

  test("nth - normal list") {
    assert(ListSolutions.nth(List(1, 2, 3), 1) === Some(2))
  }

  test("nth - empty list") {
    assert(ListSolutions.nth(List(), 1) === None)
  }

  test("length - normal list") {
    assert(ListSolutions.length(List(1, 2, 3)) === 3)
  }

  test("length - empty list") {
    assert(ListSolutions.length(List()) === 0)
  }

  test("reverse - normal list") {
    assert(ListSolutions.reverse(List(1, 2, 3)) === List(3, 2, 1))
  }

  test("reverse - empty list") {
    assert(ListSolutions.reverse(List()) === List())
  }

  test("isPalindrome - List(1, 2, 1) is a palindrome") {
    assert(ListSolutions.isPalindrome(List(1,2,1)))
  }

  test("isPalindrome - List(1, 2, 3) is not a palindrome") {
    assert(!ListSolutions.isPalindrome(List(1,2,3)))
  }

  test("isPalindrome - empty list is a palindrome") {
    assert(ListSolutions.isPalindrome(List()))
  }

  test("flatten") {
    val list = List(List(1, 1), 2, List(3, List(5, 8)))
    assert(ListSolutions.flatten(list) === List(1, 1, 2, 3, 5, 8))
  }

  test("flatMapFlatten") {
    val list = List(List(1, 1), 2, List(3, List(5, 8)))
    assert(ListSolutions.flatMapFlatten(list) === List(1, 1, 2, 3, 5, 8))
  }

  test("compress") {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(ListSolutions.compress(list) === List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("pack") {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val packedList = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
    assert(pack(list) === packedList)
  }

  test("encode") {
    val list = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    val encodedList = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    assert(encode(list) === encodedList)
  }

  test("decode") {
    val list = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    val decodedList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(decode(list) === decodedList)
  }
}