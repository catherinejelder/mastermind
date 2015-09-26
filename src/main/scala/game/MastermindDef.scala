package main.scala.game

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering
import scala.util.Random;
import scala.util.control.Exception._

/**
 * @author catherinejelder
 * Defines the interface for the mastermind game
 */
trait MastermindDef {
  
  abstract case class Color(color:String){}
  class GuessColor(color:String) extends Color(color) {}
  
  val Blue = new GuessColor("Blue")
  val Cyan = new GuessColor("Cyan")
  val Green = new GuessColor("Green")
  val Magenta = new GuessColor("Magenta")
  val Red = new GuessColor("Red")
  val Yellow = new GuessColor("Yellow")
  
  val GUESS_COLORS = List(Blue, Cyan, Green, Magenta, Red, Yellow)
  
  class ResultColor(color:String) extends Color(color) {}  
  val Black = new ResultColor("Black")
  val White = new ResultColor("White")
  val Empty = new ResultColor("Empty")
  
  case class UnknownColorException(color: String) extends Exception(color) {}
  case class IncompleteColorListException(len: String) extends Exception(len) {}
  
  private def colorsMatch(b1: GuessColor, b2: GuessColor): Boolean = b1.equals(b2)
  
  // accept a guess and master. return a sub list of both containing only the colors that do not have a full match.
   def removeFullMatches(guess: List[GuessColor], master: List[GuessColor]): (List[GuessColor], List[GuessColor]) = {
    val res: (List[GuessColor], List[GuessColor]) = removeFullMatchesAcc((guess, master), (Nil, Nil))
    (res._1.reverse, res._2.reverse)
   }
  
  // TODO: turn helper functions into nested functions
  // TOOD: create a class for the commonly used type List[GuessColor]
  @tailrec
  private def removeFullMatchesAcc(data: (List[GuessColor], List[GuessColor]), acc: (List[GuessColor], List[GuessColor])): (List[GuessColor], List[GuessColor]) = {
    data._1 match {
      case Nil => acc
      case hd_guess :: tl_guess => colorsMatch(hd_guess, data._2.head) match {
        case true => removeFullMatchesAcc((tl_guess, data._2.tail), acc)
        case false => removeFullMatchesAcc((tl_guess, data._2.tail), (hd_guess :: acc._1, data._2.head :: acc._2))
        }
    }
  }

  // accept a guess (full matches already removed). return a sub list of only the colors that do not have a partial match. 
  def removePartialMatches(guess: List[GuessColor], master: List[GuessColor]): List[GuessColor] = {
      removePartialMatchesAcc(guess, Nil, master, Nil).reverse
    }

  @tailrec
  private def removePartialMatchesAcc(guess: List[GuessColor], guessAcc: List[GuessColor], master: List[GuessColor], masterAcc: List[GuessColor]): List[GuessColor] = {
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
  }
  
  // count the number of full matches in a guess (correct color, correct position)
  def countFullMatches(guess: List[GuessColor], master: List[GuessColor]): Int = 
    guess.length - removeFullMatches(guess, master)._1.length 

  // count the number of partial matches in a guess (correct color, wrong position)
  // call on any guess
  def countPartialMatches(guess: List[GuessColor], master: List[GuessColor]): Int = {
    val safeData: (List[GuessColor], List[GuessColor]) = removeFullMatches(guess, master)
    countPartialMatchesUnsafe(safeData._1, safeData._2)    
  }

  // call only on guesses that already have full matches removed
  def countPartialMatchesUnsafe(guess: List[GuessColor], master: List[GuessColor]): Int =
    guess.length - removePartialMatches(guess, master).length

  // return true if the guess is correct (all full matches)
  def isCorrect(guess: List[GuessColor], master: List[GuessColor]): Boolean =
    countFullMatches(guess, master) == master.length
  
  // list of colors we are allowed to use in this iteration of the game
  def getPotentialColorList(numColors: Int): List[GuessColor] = GUESS_COLORS.take(numColors)
    
  // generate result list of the specified length and number of colors
  def generateResult(len: Int, numColors: Int): List[GuessColor] = {
    val rand = new Random()
    val colorList = getPotentialColorList(numColors)
    @tailrec
    def generateResultAcc(length: Int, acc: List[GuessColor]): List[GuessColor] = {
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
  
  // generate master list of the specified length and number of colors  
  def generateMaster(numColors: Int, length: Int): List[GuessColor] = {
    val rand = new Random()
    val colorList = getPotentialColorList(numColors)
    @tailrec
    def generateMasterAcc(length: Int, acc: List[GuessColor]): List[GuessColor] = {
      length match {
      case 0 => acc
      case _ => generateMasterAcc(length - 1, colorList(rand.nextInt(numColors)) :: acc)
      }
    }
    generateMasterAcc(length, List())
  }

  // get color for color name
  @throws(classOf[UnknownColorException])
  def getColor(name: String, numColors: Int): GuessColor = {
    val potentialColorList = getPotentialColorList(numColors)
    val index = potentialColorList.map(c => c.color.take(1).toLowerCase()).indexOf(name.take(1).toLowerCase()) // TODO: check more than the first character of the color name
    index match {
      case -1 => throw new UnknownColorException(name)
      case _ => {
        potentialColorList.lift(index).get
      }
    }
  }
  
  // turn a user-defined string into a list of colors
  @throws(classOf[IncompleteColorListException])
  def getColorList(names: String, len: Int, numColors: Int): List[GuessColor] = {
    var colorList: List[GuessColor] = names.split(" ").toList.map(c => getColor(c, numColors))
    if (colorList.length != len) {
      throw new IncompleteColorListException(colorList.length.toString) 
    }
    colorList
  }
  
  // get pretty string to represent a color
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
  
  // get result colors for a guess
  def getFeedbackColors(guess: List[GuessColor], master: List[GuessColor]): List[ResultColor] = {
    val fullMatches = countFullMatches(guess, master)
    val partialMatches = countPartialMatches(guess, master)
    List.fill(fullMatches)(Black) ::: List.fill(partialMatches)(White)
  }
}
