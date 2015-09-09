import main.scala.game._
import main.scala.game.MastermindDef

object TestGame {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(124); 
  println("Welcome to the Scala worksheet");$skip(12); 
  val x = 1;System.out.println("""x  : Int = """ + $show(x ));$skip(16); 
  val y = 2 * x;System.out.println("""y  : Int = """ + $show(y ));$skip(44); 
  
  val game = new MastermindGame(4, 4, 8);System.out.println("""game  : main.scala.game.MastermindGame = """ + $show(game ));$skip(39); 
  val example = game.getExampleGuess();System.out.println("""example  : List[TestGame.game.GuessColor] = """ + $show(example ));$skip(27); 
  val master = game.master;System.out.println("""master  : List[TestGame.game.GuessColor] = """ + $show(master ));$skip(41); val res$0 = 
  game.countFullMatches(example, master);System.out.println("""res0: Int = """ + $show(res$0))}
  
}
