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
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceHelper(chars: List[Char], numOfParens: Int): Boolean = {
        if (numOfParens < 0) false
        else if (chars.isEmpty) numOfParens == 0
        else if (chars.head == '(') balanceHelper(chars.tail, numOfParens + 1)
        else if (chars.head == ')') balanceHelper(chars.tail, numOfParens - 1)
        else balanceHelper(chars.tail, numOfParens)
      }
      balanceHelper(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def iter(money: Int, coins: List[Int], ways: Int): Int = {
        if (money == 0 || coins.isEmpty) ways
        else if (money - coins.head < 0) iter(money, coins.tail, ways)
        else if (money - coins.head == 0) iter(money, coins.tail, ways + 1)
        else iter(money - coins.head, coins, ways) + iter(money, coins.tail, 0)
      }
      iter(money, coins, 0)
    }
  }
