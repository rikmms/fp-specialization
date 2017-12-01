package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  import Main.balance

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }

  test("balance: '(if (zero? x)' is unbalanced") {
    assert(!balance("(if (zero? x)".toList))
  }

  test("balance: '())----sdada()' is unbalanced") {
    assert(!balance("())----sdada()".toList))
  }

  test("balance: '(((((())))' is unbalanced") {
    assert(!balance("(((((())))".toList))
  }

  test("balance: '((())()a)b' is balanced") {
    assert(balance("((())()a)b".toList))
  }

  test("balance: empty list is balanced") {
    assert(balance("".toList))
  }

  test("balance: 'zero? x' is balanced") {
    assert(balance("zero? x".toList))
  }
}
