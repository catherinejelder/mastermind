package main.scala.game

import scala.io.Source
import main.scala.game.MastermindDef

object Main {
  def main(args: Array[String]) {
        Console.println("Welcome to mastermind!")
        println("the best game")
//        val name = readLine("How many colors do you want to use? ")
        print("How many colors do you want to use? ")
        val numColors = readInt()
        print("How many spots do you want to guess? ")
        val len = readInt()
        
        println("starting game with " + numColors + " colors and " + len + " spots")
        
        class MastermindGame(numC: Int, len: Int) extends MastermindDef {
          val length: Int = len
          val numColors: Int = numC
          
          val master = generateMaster(numColors, length)
          
          var feedbackCumulative: String = ""
          
          def getNumberOfGuesses(): Int = feedbackCumulative.filter(_ == '\n').length
          
          def getFeedback(guess: List[GuessColor]): String = {
            val feedback = getFeedbackForGuess(guess)
            feedbackCumulative = feedback + "\n" + feedbackCumulative
            feedbackCumulative
          }
          
          def getFeedbackForGuess(guess: List[GuessColor]): String = {
            guess.map(c => getConsoleStrForColor(c)).foldLeft("")(_ + _) + " |" + getFeedbackColors(guess, master).map(c => getConsoleStrForColor(c)).foldLeft("")(_ + _)
          }
          
          def isCorrect(guess: List[GuessColor]): Boolean = isCorrect(guess, master)
        }
        
        val game = new MastermindGame(numColors, len)
 
        var guess = game.getColorList("")
        var feedback = ""
        
        do {
          val guessStr = readLine("guess: ")
//          println("your guess: " + guessStr)
          
          guess = game.getColorList(guessStr)
          val master = game.master
          feedback = game.getFeedback(guess)
          
//          println("your guess in colors: " + guess)
          println("master: " + master)
          println("feedback so far, " + game.getNumberOfGuesses() + " guesses: \n" + feedback)
        } while (!game.isCorrect(guess))
        
        println("you won!! \n" + feedback)
  }
}