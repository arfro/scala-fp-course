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
   if(c == 0 || c == r) 1
    else if(c > r) 0
    else pascal(c-1, r-1) + pascal(c, r-1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(ch: List[Char], accum: Int): Boolean = {
      if(ch.isEmpty) accum == 0
      else{
        //if head is (
        if(ch.head == '(') loop(ch.tail, accum + 1)
        else {
          //if head is )
          if (ch.head == ')') {
            if(accum < 0) false
            else loop(ch.tail, accum - 1)
          }
          //if head is neither ( nor )
          else loop(ch.tail, accum)
        }
      }
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(amt: Int, list: List[Int]): Int = {
        if (amt == 0) 1
        else if (amt < 0 || list.isEmpty) 0
        else count(amt - list.head, list) + count(amt, list.tail)
      }
      count(money, coins)
  }
  }
