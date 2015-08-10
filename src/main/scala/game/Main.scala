package main.scala.game

import scala.io.Source
import java.lang.Integer
//import main.scala.game._

/**
 * @author catherinejelder
 * Provides a UI for playing the mastermind game
 */

object Main {
  def main(args: Array[String]) {
        println("\nwelcome to mastermind!")
        println("guess the colors of a secret row of dots to win!")
        
        print("how many dots do you want to guess? ")
        val lenLine = readLine()
        var len = 4
        try {
          len = Integer.parseInt(lenLine)
        } catch {
          case e: NumberFormatException => print("hmm, I don't recognize that number. Let's use " + len + " dots.\n")
        }
        
        print("how many colors do you want to use? ")
        val numColorsLine = readLine()
        var numColors = 6
        try {
          val numC = Integer.parseInt(numColorsLine)
          if (numC > numColors) {
            print("I only know " + numColors + " colors, let's go with that :)\n")
          } else {
            numColors = numC
          }
        } catch {
          case e: NumberFormatException => print("hmm, I don't recognize that number. Let's use " + numColors + " colors.\n")
        }
        
        print("how many guesses do you want? ")
        val numGuessesLine = readLine()
        var numGuesses = 8
        try {
          numGuesses = Integer.parseInt(numGuessesLine)
        } catch {
          case e: NumberFormatException => print("Hmm, I don't recognize that number. Let's give you " + numGuesses + " guesses.\n")
        }
        
        println("starting game with " + numColors + " colors, " + len + " dots, " + numGuesses + " guesses\n")
        println("guess the colors of a secret list of " + len + " dots to win!")
        
        val game = new MastermindGame(numColors, len, numGuesses)
        
        // TODO: take lower level functionality like this into another class
        print("dots you can use: \n" + "color\tname\tnickname\n"+ game.getPotentialColorList().map(c => game.getConsoleStrForColor(c) + "\t" + c.color.toLowerCase() + "\t" + c.color.take(1).toLowerCase() + "\n").foldLeft("")(_ + _))
        println("to guess, enter the name or nickname of each dot, separated by spaces")
        val example = game.getExampleGuess()
        println("for example, a guess of " + example.map(c => c.color.take(1).toLowerCase()).foldLeft("")(_ + _ + " ") + "means" + example.map(c => game.getConsoleStrForColor(c)).foldLeft("")(_ + _))
        
        def getGuessFromUser(): List[game.GuessColor] = {
          var guess: List[game.GuessColor] = Nil
          var acceptableGuess = false
          do {
            val guessStr = readLine("guess: ") 
            
            try {
              guess = game.getColorList(guessStr)
              acceptableGuess = true
            } catch {
              case game.UnknownColorException(color) => println("hmm, I don't recognize the color " + color)
              case game.IncompleteColorListException(length) => println("hmm, we need a guess of " + len + " dots, not " + length)
            }
          } while (!acceptableGuess)
          guess
        }
        
        var feedback = ""
        var firstGuess = true
        var guess: List[game.GuessColor] = Nil
        
        do {
           guess = getGuessFromUser()
          feedback = game.getFeedback(guess)  
          
          var guessLabel = "guesses"
          if (firstGuess) {
            guessLabel = "guess"
          }
          println("feedback so far, " + game.getNumberOfGuessesSoFar() + " " + guessLabel + ": \n" + feedback)
          
          if (firstGuess) {
            println("in the feedback, a black dot means that one of your dots is the right color in the right position. A white dot means that one of your dots is the right color in the wrong position.\n")  
          }
          firstGuess = false
        } while (!game.isOver(guess))

        if (game.isCorrect(guess)) {
          println("you won!! \n")          
        } else {
          println("you lost after " + game.getNumberOfGuessesSoFar() + " guesses :(")
          println("the solution was: " + game.master.map(c => game.getConsoleStrForColor(c)).foldLeft("")(_ + _))
        }
  }
}