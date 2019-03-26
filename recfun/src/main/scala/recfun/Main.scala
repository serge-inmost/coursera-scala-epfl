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
		if ((c == 1) || (c == r)) 1
		else pascal(c-1,r-1) + pascal(c,r-1)
	}		
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
		def loop(chars: List[Char], nOpen: Int): Boolean = {
			if (chars.isEmpty && nOpen == 0) true
			else if ((chars.isEmpty && nOpen != 0) || nOpen < 0) false
			else chars.head match {
				case '(' => loop(chars.tail, nOpen +1)
				case ')' => loop(chars.tail, nOpen -1)
				case _ => loop(chars.tail, nOpen)
			}
		}
		loop(chars,0)
	}
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
		if ((money < 0) || coins.isEmpty) 0
		else if (money==0) 1
		else countChange(money - coins.head,coins) + countChange(money,coins.tail)
	}
  
}
