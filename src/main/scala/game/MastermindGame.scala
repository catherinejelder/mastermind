package main.scala.game

/**
 * @author catherinejelder
 * Provides an API for interacting with the mastermind game
 */

class MastermindGame(numColors: Int, length: Int, numGuesses: Int) extends MastermindDef {

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
  
  def getColorList(names: String): List[GuessColor] = getColorList(names, length)
  
  def getPotentialColorList(): List[GuessColor] = getPotentialColorList(numColors)
  def getExampleGuess(): List[GuessColor] = generateMaster(numColors, length)
  
  def isCorrect(guess: List[GuessColor]): Boolean = isCorrect(guess, master)
  private def isLost(): Boolean = getNumberOfGuessesSoFar() >= numGuesses
  def isOver(guess: List[GuessColor]): Boolean = isCorrect(guess) || isLost()
}
