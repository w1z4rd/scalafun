package recfun

import common._
import scala.actors.threadpool.Arrays

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
    if (isPointOutsideTriangle(c, r)) {
      throw new NoSuchElementException("requested point outside of triangle")
    } else {
      computePascal(c, r)
    }
  }

  def computePascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || (c == r)) {
      1
    } else {
      val columnParentOne = c - 1
      val columnParentTwo = c
      val rowParent = r - 1
      val partenOne = computePascal(columnParentOne, rowParent)
      val partenTwo = computePascal(columnParentTwo, rowParent)
      partenOne + partenTwo
    }
  }

  def isPointOutsideTriangle(c: Int, r: Int): Boolean = {
    if (c < 0 || r < 0 || c > r) {
      true
    } else {
      false
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty || hasNoParentheses(chars)) {
      true
    } else {
      checkIfIsBalanced(chars, 0, 0)
    }
  }

  def checkIfIsBalanced(chars: List[Char], openCount: Int, closedCount: Int): Boolean = {
    if (chars.isEmpty) {
      openCount == closedCount
    } else {
      if (closedCount > openCount) {
        false
      } else {
        chars.head match {
          case '(' => checkIfIsBalanced(chars.tail, openCount + 1, closedCount)
          case ')' => checkIfIsBalanced(chars.tail, openCount, closedCount + 1)
          case _ => checkIfIsBalanced(chars.tail, openCount, closedCount)
        }
      }
    }
  }

  def hasNoParentheses(chars: List[Char]): Boolean = {
    if (chars.contains('(') || chars.contains(')')) {
      false
    } else {
      true
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}

