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
  def pascal(column: Int, depth: Int): Int = {
    def isValidPosition(column: Int, depth: Int): Boolean = {
      column <= depth
    }

    def isTrivialCase(column: Int, depth: Int): Boolean = {
      column == 0 || depth == 1
    }

    if (!isValidPosition(column, depth)) 0
    else if (isTrivialCase(column, depth)) 1
    else {
      val aboveLeft: Int = pascal(column - 1, depth - 1)
      val aboveRight: Int = pascal(column, depth - 1)

      aboveLeft + aboveRight
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countParenthesis(balanceAcc: Int, chars: List[Char]): Boolean = {
      if (balanceAcc < 0) false
      else if (chars.isEmpty) balanceAcc == 0
      else if (chars.head == '(') countParenthesis(balanceAcc+1, chars.tail)
      else if (chars.head == ')') countParenthesis(balanceAcc-1, chars.tail)
      else countParenthesis(balanceAcc, chars.tail)
    }

    countParenthesis(0, chars)
  }

  /**
   * Exercise 3
   *
   * @see Algorithm from http://www.algorithmist.com/index.php/Coin_Change
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
