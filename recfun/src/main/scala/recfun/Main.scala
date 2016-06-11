package recfun

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def findClosing(chars: List[Char], depth: Int): Boolean = {
      if (depth == 0) balance(chars) // the parenthesis are closed, return to balance
      else if (chars.isEmpty) false // we arrived to the last char with positive depth
      // if we find a parenthesis, increase depth
      else if (chars.head == '(') findClosing(chars.tail, depth + 1)
      // if a parenthesis is closed, decrease depth
      else if (chars.head == ')') findClosing(chars.tail, depth - 1)
      else findClosing(chars.tail, depth) // nothing found, keep looking
    }

    if (chars.isEmpty) true
    else if (chars.head == ')') false
    else if (chars.head == '(') findClosing(chars.tail, 1)
    else balance(chars.tail)
  }

  /**
    * Exercise 3
    */

  def countChange(money: Int, coins: List[Int]): Int = {

    def accumulate(acc: Int, toBeTried: List[Int]): Int = {
      if (acc == money) 1
      else if (acc > money || toBeTried.isEmpty) 0
      else accumulate(acc + toBeTried.head, toBeTried) + accumulate(acc, toBeTried.tail)
    }

    if (money == 0) 0
    else accumulate(0, coins)
  }
}
