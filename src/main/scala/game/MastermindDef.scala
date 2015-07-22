package main.scala.game

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering
/**
 * @author celder
 * Define the interface for the mastermind game
 */
trait MastermindDef {
  
   abstract case class Color(color:String) extends Ordering[Color]{
//      def compare(that: Color): Int = this.color.compareTo(that.color)
      def compare(x: Color, y: Color): Int = x.color.compareTo(y.color)     
   }

   class GuessColor(color:String) extends Color(color) {
      override def compare(x: Color, y: Color): Int = x.color.compareTo(y.color)
  }
   
  val Blue = new GuessColor("Blue")
  val Green = new GuessColor("Green")
  val Orange = new GuessColor("Orange")
  val Purple = new GuessColor("Purple")
  val Red = new GuessColor("Red")
  val Yellow = new GuessColor("Yellow")

  sealed class ResultColor(color:String) extends Color(color) {

  }
  val Black = new ResultColor("Black")
  val White = new ResultColor("White")
  val Empty = new ResultColor("Empty")
  
//  abstract class ColorList extends List[Color]
  
  // TODO: require that guesses and results are the correct length
//  class Guess(colors: List[GuessColor]) extends ColorList
//  val Guess: List[GuessColor]
//    class Guess(colors: List[GuessColor]) extends ColorList[GuessColor]
  
//  class Guess(colors: List[GuessColor]) extends ColorList  
//  class Result(colors: List[ResultColor]) extends ColorList
//    class Result extends List[ResultColor]
//  class MasterLists(guess: Guess, master: Guess)
  
  case class MasterLists(guess: List[GuessColor], master: List[GuessColor]) 
  
  private def colorsMatch(b1: GuessColor, b2: GuessColor): Boolean = b1.equals(b2)
  
  // accept a guess. return a sub list of only the colors that do not have a full match.
  def removeFullMatches(guess: List[GuessColor], master: List[GuessColor]): List[GuessColor] = {
    removeFullMatchesAcc(guess, master, Nil)
  }
    
  @tailrec
  private def removeFullMatchesAcc(guess: List[GuessColor], master: List[GuessColor], acc: List[GuessColor]): List[GuessColor] = 
    guess match {
    case Nil => acc
    case hd_guess :: tl_guess => colorsMatch(hd_guess, master.head) match {
      case true => removeFullMatchesAcc(tl_guess, master.tail, acc)
      case false => removeFullMatchesAcc(tl_guess, master.tail, hd_guess :: acc)
    }
  }
  
  // accept a guess (full matches already removed). return a sub list of only the colors that do not have a partial match.
  def removePartialMatches(guess: List[GuessColor], master: List[GuessColor]) = {
      removePartialMatchesAcc(guess, Nil, master, Nil)
    }

  @tailrec
  private def removePartialMatchesAcc(guess: List[GuessColor], guessAcc: List[GuessColor], master: List[GuessColor], masterAcc: List[GuessColor]): List[GuessColor] =
//    println("removePartialMatchesAcc called with guess:" + guess + ", guessAcc: " + guessAcc + ", master: " + master + ", masterAcc: " + masterAcc)
    guess match {
    case Nil => guessAcc
    case hd_guess :: tl_guess => master match {
      case Nil => removePartialMatchesAcc(tl_guess, hd_guess :: guessAcc, masterAcc, Nil)
      case hd_master :: tl_master => colorsMatch(hd_guess, hd_master) match {
        case true => removePartialMatchesAcc(tl_guess, guessAcc, masterAcc ::: tl_master, Nil)
        case false => removePartialMatchesAcc(guess, guessAcc, tl_master, hd_master :: masterAcc)
      }
    }
  }
  
  def countFullMatches(guess: List[GuessColor], master: List[GuessColor]): Int = 
    guess.length - removeFullMatches(guess, master).length
   
  // call on any guess
  def countPartialMatches(guess: List[GuessColor], master: List[GuessColor]): Int =
    countPartialMatchesUnsafe(removeFullMatches(guess, master), master)
    
  // call only on guesses that already have full matches removed
  def countPartialMatchesUnsafe(guess: List[GuessColor], master: List[GuessColor]): Int =
    guess.length - removePartialMatches(guess, master).length
  
  def isCorrect(guess: List[GuessColor], master: List[GuessColor]): Boolean =
    countFullMatches(guess, master) == master.length
    
//  def getResult(guess: List[GuessColor], master: List[GuessColor]): List[ResultColor] =
    
    
}