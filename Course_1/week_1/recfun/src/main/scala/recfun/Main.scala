package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c <= 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      
      def isBalanced(chars: List[Char], accOpens: Int): Boolean = {
        if (chars.isEmpty) accOpens == 0
        else if (accOpens < 0) false
        else {
          val incr = if (chars.head == '(') 1 else if(chars.head == ')') - 1 else 0
          isBalanced(chars.tail, accOpens + incr)
        }
      }

      isBalanced(chars, 0)
    }

  /**
   * Exercise 3
   * structure and interpretation of computer programs solutions - chapter 1.2.2
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money == 0) 1
      else if (money < 0) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
