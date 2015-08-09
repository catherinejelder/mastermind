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
        print("How many guesses do you want? ")
        val numGuesses = readInt()
        
        println("starting game with " + numColors + " colors, " + len + " spots, " + numGuesses + " guesses")
        
        class MastermindGame(numC: Int, len: Int, numG: Int) extends MastermindDef {
          val length: Int = len
          val numColors: Int = numC
          val numGuesses: Int = numG
          
          val master = generateMaster(numColors, length)
          
          var feedbackSoFar: String = ""
          
          def getNumberOfGuessesSoFar(): Int = feedbackSoFar.filter(_ == '\n').length
          
          def getFeedback(guess: List[GuessColor]): String = {
            val feedback = getFeedbackForGuess(guess)
            feedbackSoFar = feedback + "\n" + feedbackSoFar
            feedbackSoFar
          }
          
          def getFeedbackForGuess(guess: List[GuessColor]): String = {
            guess.map(c => getConsoleStrForColor(c)).foldLeft("")(_ + _) + " |" + getFeedbackColors(guess, master).map(c => getConsoleStrForColor(c)).foldLeft("")(_ + _)
          }
          
          def isCorrect(guess: List[GuessColor]): Boolean = isCorrect(guess, master)
          private def isLost(): Boolean = getNumberOfGuessesSoFar() >= numGuesses
          def isOver(guess: List[GuessColor]): Boolean = isCorrect(guess) || isLost()
        }
        
        val game = new MastermindGame(numColors, len, numGuesses)
 
        var guess = game.getColorList("")
        var feedback = ""
        
        do {
          val guessStr = readLine("guess: ")
//        println("your guess: " + guessStr)
          
          guess = game.getColorList(guessStr)
          val master = game.master
          feedback = game.getFeedback(guess)
          
//        println("your guess in colors: " + guess)
          println("master: " + master)
          println("feedback so far, " + game.getNumberOfGuessesSoFar() + " guesses: \n" + feedback)
        } while (!game.isOver(guess))
        
        // game's over, how did we do?
        if (game.isCorrect(guess)) {
          println("you won!! \n")          
        } else {
          println("you lost after " + game.getNumberOfGuessesSoFar() + " guesses :(")
        }
  }
}