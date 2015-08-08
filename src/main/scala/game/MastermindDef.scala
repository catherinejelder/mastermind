package main.scala.game

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering
import scala.util.Random;
import scala.util.control.Exception._

/**
 * @author celder
 * Define the interface for the mastermind game
 */
trait MastermindDef {
  
   abstract case class Color(color:String) extends Ordering[Color]{
//      def compare(that: Color): Int = this.color.compareTo(that.color)
      def compare(x: Color, y: Color): Int = x.color.compareTo(y.color) 
//      def getColor() = color
   }

   class GuessColor(color:String) extends Color(color) {
      override def compare(x: Color, y: Color): Int = x.color.compareTo(y.color)
  }
  
//  val length
//  val setLength(len: Int): Unit() = {length = len}
//  val numColors
//  val setNumColors(num: Int): Unit() = {numColors = num}
  
  val Blue = new GuessColor("Blue")
  val Cyan = new GuessColor("Cyan")
  val Green = new GuessColor("Green")
  val Magenta = new GuessColor("Magenta")
  val Red = new GuessColor("Red")
  val Yellow = new GuessColor("Yellow")
  
  // TODO: let user set number of colors
  
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
  // TODO: fix this so that it returns a different guess as well as different master
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
  val GUESS_COLORS = List(Blue, Cyan, Green, Magenta, Red, Yellow)
  
  def getPotentialColorList(numColors: Int): List[GuessColor] = GUESS_COLORS.take(numColors)
    
  // generate result list of the specified length and number of colors
  def generateResult(len: Int, numColors: Int): List[GuessColor] = {
    val rand = new Random()
    val colorList = getPotentialColorList(numColors)
//    println("colorList: " + colorList)
    @tailrec
    def generateResultAcc(length: Int, acc: List[GuessColor]): List[GuessColor] = {
//      println("generateResultAcc called with length: " + length + ", acc: " + acc)
      length match {
      case 0 => acc
      case _ => generateResultAcc(length - 1, colorList(rand.nextInt(numColors)) :: acc)
      }
    }
    numColors match {
      case 0 => List()
      case _ => generateResultAcc(len, List())
    }
  }
  
    // generate result list of the specified length and number of colors
//  val master(numColors, length): List[GuessColor] = generateMaster(numColors, length) 
  
  def generateMaster(numColors: Int, length: Int): List[GuessColor] = { // TODO: only call once!
    val rand = new Random()
    val colorList = getPotentialColorList(numColors)
//    println("colorList: " + colorList)
    @tailrec
    def generateMasterAcc(length: Int, acc: List[GuessColor]): List[GuessColor] = {
//      println("generateResultAcc called with length: " + length + ", acc: " + acc)
      length match {
      case 0 => acc
      case _ => generateMasterAcc(length - 1, colorList(rand.nextInt(numColors)) :: acc)
      }
    }
    generateMasterAcc(length, List())
  }

  def getColor(name: String): GuessColor = {
    name.take(1).toLowerCase() match {
      case "b" => Blue
      case "c" => Cyan
      case "g" => Green
      case "m" => Magenta
      case "r" => Red
      case "y" => Yellow
//      case _ => throw Exception("I don't know this color: " + name)
    }
  }
  
  def getColorList(names: String): List[GuessColor] = {
    @tailrec
    def getColorListAcc(names: List[String], acc: List[GuessColor]): List[GuessColor] = {
      names match {
        case Nil => acc
        case c :: rest => getColorListAcc(rest, getColor(c) :: acc)
      }
    }    
    getColorListAcc(cleanColorNames(names), Nil).reverse
  }
  
  // TODO: check length of names
  def cleanColorNames(names: String): List[String] = {
    names match {
      case "" => Nil
      case _ => names.split(" ").toList
    }
  }
   
//  def getGuessFeedbackAcc(guess: List[GuessColor], acc: String): String = {
//    guess match {
//      case Nil => acc
//      case g :: rest => 
//    }
//  }
  
  def getConsoleStrForColor(c: Color): String = {
    val asciiNum = 79
    val aChar = asciiNum.toChar
    c match {
      case Blue => " " + Console.BLUE + aChar + Console.RESET
      case Cyan => " " + Console.CYAN + aChar + Console.RESET
      case Green => " " + Console.GREEN + aChar + Console.RESET
      case Magenta => " " + Console.MAGENTA + aChar + Console.RESET
      case Red => " " + Console.RED + aChar + Console.RESET
      case Yellow => " " + Console.YELLOW + aChar + Console.RESET
      
      case Black => " " + Console.BOLD + Console.BLACK + aChar + Console.RESET
      case White => " " + Console.WHITE + aChar + Console.RESET
      case Empty => ""
      }
  }
  
  def getFeedbackColors(guess: List[GuessColor], master: List[GuessColor]): List[ResultColor] = {
    val fullMatches = countFullMatches(guess, master)
    val partialMatches = countPartialMatches(guess, master)
    List.fill(fullMatches)(Black) ::: List.fill(partialMatches)(White)
  }
}