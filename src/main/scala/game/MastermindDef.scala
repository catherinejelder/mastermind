package main.scala.game

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering
/**
 * @author celder
 * Define the interface for the mastermind game
 */
trait MastermindDef {
  
//  object Color extends Enumeration {
//    type Color = Value
//    val Blue = Value("Blue")
//    val Green = Value("Green")
//    val Orange = Value("Orange")
//    val Purple = Value("Purple")
//    val Red = Value("Red")
//    val Yellow = Value("Yellow")
//   
//    val Black = Value("Black")
//    val White = Value("White")    
//  }
  
//  sealed abstract class Color extends Ordered[Color] {
//      def compare(that: Color): Int = this.toString.compareTo(that.toString)
////    def compare(that: Color): Int = this.toString compare that.toString
//  }
  
//    abstract class Color extends Enumeration {
//      type Color = Value
//      def compare(that: Color): Int = this.toString.compareTo(that.toString)
//  }
   abstract case class Color(color:String) extends Ordering[Color]{
//      def compare(that: Color): Int = this.color.compareTo(that.color)
      def compare(x: Color, y: Color): Int = x.color.compareTo(y.color)     
   }
    // was abstract
   class GuessColor(color:String) extends Color(color) {
      override def compare(x: Color, y: Color): Int = x.color.compareTo(y.color)     

//    type GuessColor = Value
//    val Blue, Green, Orange, Purple, Red, Yellow = Value
     
//    val Blue = Value
//    val Green = Value("Green")
//    val Orange = Value("Orange")
//    val Purple = Value("Purple")
//    val Red = Value("Red")
//    val Yellow = Value("Yellow")
  }
  
//  object GuessColors extends GuessColor
  val Blue = new GuessColor("Blue")
  val Green = new GuessColor("Green")
  val Orange = new GuessColor("Orange")
  val Purple = new GuessColor("Purple")
  val Red = new GuessColor("Red")
  val Yellow = new GuessColor("Yellow")

  sealed class ResultColor(color:String) extends Color(color) {
//    val Black, White, Empty = Value
    
//    val Black = Value("Black")
//    val White = Value("White")
//    val Empty = Value("Empty")
  }
  
//  case object Blue extends GuessColor
//  case object Green extends GuessColor
//  case object Orange extends GuessColor
//  case object Purple extends GuessColor
//  case object Red extends GuessColor
//  case object Yellow extends GuessColor
//  
//  case object Black extends ResultColor  // correct color, correct position
//  case object White extends ResultColor  // correct color, wrong position
//  case object Empty extends ResultColor  // wrong color, wrong position
//  
//  val guess1 = List(Blue, Blue, Green, Orange)
//  val master1 = List(Blue, Blue, Orange, Purple)
//  val test1 = countFullMatches(guess1, master1)
//  
  
//  sealed abstract class Ball
//  class GuessBall(c: GuessColor) extends Ball
//  class ResultBall(c: ResultColor) extends Ball
  
  sealed abstract class ColorList
  
  // TODO: require that guesses and results are the correct length
//  class Guess(colors: List[GuessColor]) extends ColorList
//  val Guess: List[GuessColor]
  
//  class Guess(colors: List[GuessColor]) extends ColorList  
//  class Result(colors: List[ResultColor]) extends ColorList
  
//  class MasterLists(guess: Guess, master: Guess)
  
  case class MasterLists(guess: List[GuessColor], master: List[GuessColor]) 
  
  // a guess is correct if all balls in result are black
//  @tailrec
//  private def correct(colors: List[ResultColor]): Boolean =
//    colors match {
//    case Nil => true
//    case hd :: tl => if (hd.equals(Black)) correct(tl) else false
//  }

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
//    if (colorsMatch(guess.head, master.head)) 
//      removeFullMatchesAcc(guess.tail, master.tail, acc)
//    else
//      removeFullMatchesAcc(guess.tail, master.tail, guess.head :: acc) // TODO: maintain ordering by appending to end of list
  
  
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
    
//  private def getResult(guess: List[GuessColor], master: List[GuessColor]): List[ResultColor] =

}