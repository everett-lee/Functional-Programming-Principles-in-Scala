package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r < 2 || c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def helper(chars: List[Char], seen: List[Char]): Boolean = {
      if (chars.isEmpty && seen.isEmpty) true
      else if (chars.isEmpty && seen.nonEmpty ||
        chars.head == ')' && seen.isEmpty) false
      else if (chars.head == '(') helper(chars.tail, ')' :: seen)
      else if (chars.head == ')') helper(chars.tail, seen.tail)
      else helper(chars.tail, seen)
    }

    if (chars.isEmpty) true
    else helper(chars, List())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def helper(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else {
        helper(money - coins.head, coins) + helper(money, coins.tail)
      }
    }

    if (money <= 0) 0
    else helper(money, coins)
  }
}
