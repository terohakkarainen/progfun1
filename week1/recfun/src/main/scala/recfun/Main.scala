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
  def pascal(c: Int, r: Int): Int = {
    if (isFirstOrSecondRow(r) || isFirstColumn(c) || isLastColumn(c, r)) 1
    else leftParent(c, r) + rightParent(c, r)
  }

  def leftParent(c: Int, r: Int): Int = {
    pascal(c - 1, r - 1)
  }

  def rightParent(c: Int, r: Int): Int = {
    pascal(c, r - 1)
  }

  def isFirstOrSecondRow(r: Int): Boolean = r <= 1

  def isFirstColumn(c: Int): Boolean = c == 0

  def isLastColumn(c: Int, r: Int): Boolean = c == r

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def loop(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else {
        if (chars.head == '(') loop(chars.tail, open + 1)
        else if (chars.head == ')') {
          if (open == 0) false
          else loop(chars.tail, open - 1)
        }
        else loop(chars.tail, open)
      }
    }

    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    assume(money >= 0)
    for (c <- coins) assume(c > 0)

    def loop(money: Int, coins: List[Int]): Int = {
      if (moneyUnderflowOrNoCoinsLeft(money, coins)) 0
      else if (allMoneyChanged(money)) 1
      else headCoinNotUsed(money, coins) + headCoinUsed(money, coins)
    }

    def moneyUnderflowOrNoCoinsLeft(money: Int, coins: List[Int]): Boolean = money < 0 || coins.isEmpty

    def allMoneyChanged(money: Int): Boolean = money == 0

    def headCoinNotUsed(money: Int, coins: List[Int]): Int = loop(money, coins.tail)

    def headCoinUsed(money: Int, coins: List[Int]): Int = loop(money - coins.head, coins)

    if (money == 0 || coins.isEmpty) 0
    else loop(money, coins)
  }
}
