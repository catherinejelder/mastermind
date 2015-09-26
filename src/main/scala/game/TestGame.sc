import main.scala.game._
import main.scala.game.MastermindDef

object TestGame {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val x = 1                                       //> x  : Int = 1
  val y = 2 * x                                   //> y  : Int = 2
  
  val game = new MastermindGame(4, 4, 8)          //> game  : main.scala.game.MastermindGame = main.scala.game.MastermindGame@1e67
                                                  //| b872
  val example = game.getExampleGuess()            //> example  : List[TestGame.game.GuessColor] = List(Color(Cyan), Color(Blue), C
                                                  //| olor(Blue), Color(Cyan))
  val master = game.master                        //> master  : List[TestGame.game.GuessColor] = List(Color(Cyan), Color(Green), C
                                                  //| olor(Cyan), Color(Cyan))
  game.countFullMatches(example, master)          //> res0: Int = 2
  game.countPartialMatches(example, master)       //> res1: Int = 0
  
}