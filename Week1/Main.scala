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
      if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def cbalance(chars: List[Char],left: Int): Boolean = {
        if (chars.isEmpty) left == 0
        else {
          if (chars.head == ')') {
            left > 0 && cbalance(chars.tail, left - 1)
          }
          else if (chars.head == '(') {
            cbalance(chars.tail, left + 1)
          }
          else {
            cbalance(chars.tail, left)
          }
        }
      }
      cbalance(chars,0)
      }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def degcount(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else degcount(money, coins.tail) + degcount(money - coins.head, coins)
    }

    degcount(money, coins)
  }
}
