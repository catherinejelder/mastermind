import game._

object TestGame {
  println("Welcome to the Scala worksheet")
  val x = 1
  val y = 2 * x
  
  val guess1 = List(Blue, Blue, Green, Orange)
  val master1 = List(Blue, Blue, Orange, Purple)
  
  val test1 = countFullMatches(guess1, master1)
  
  }