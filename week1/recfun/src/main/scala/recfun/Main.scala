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
    if(isFirstOrSecondRow(r) || isFirstColumn(c) || isLastColumn(c, r)) 1
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
  def balance(chars: List[Char]): Boolean = ???

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
