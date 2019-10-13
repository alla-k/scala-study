package recfun

import scala.collection.mutable

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
    if (c > r) 0
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true
    var balanced = true
    var arrayInit = mutable.Stack[Char]()

    def calc(chars: List[Char]): Any = {
      if (chars.isEmpty) return
      if (chars.head == '(') {
        arrayInit.push(chars.head)
      }
      else if (chars.head == ')') {
        if (!arrayInit.isEmpty)
          arrayInit.pop()
        else balanced = false
      }
      calc(chars.tail)
    }

    calc(chars)
    balanced && arrayInit.isEmpty

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)

  }

}

