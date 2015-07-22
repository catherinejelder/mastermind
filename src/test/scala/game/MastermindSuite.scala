package test.scala.game

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import main.scala.game.MastermindDef

@RunWith(classOf[JUnitRunner])
class MastermindSuite extends FunSuite {
      
    // all full matches
     trait Move1a extends MastermindDef {
      val guess = List(Orange, Orange, Orange, Orange)
      val guessWithoutFullMatches = List()
      val master = List(Orange, Orange, Orange, Orange)
    }
    
    test("1a: all full matches") {
      new Move1a {      
        assert(removeFullMatches(guess, master) === List()) // TODO: sort lists!
        assert(countFullMatches(guess, master) === 4)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List())
        assert(countPartialMatches(guess, master) === 0)
        assert(isCorrect(guess, master) == true)
      }
    }
    
    // all partial matches
    trait Move2a extends MastermindDef {
      val guess = List(Orange, Orange, Blue, Green)
      val guessWithoutFullMatches = guess
      val master = List(Blue, Green, Orange, Orange)
    }
    
    test("2a: all partial matches") {
      new Move2a {
        assert(removeFullMatches(guess, master) === List(Orange, Orange, Blue, Green).reverse) // TODO: sort lists!
        assert(countFullMatches(guess, master) === 0)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List())
        assert(countPartialMatches(guess, master) === 4)
        assert(isCorrect(guess, master) == false)
      }
    }
    
    trait Move2b extends MastermindDef {
      val guess = List(Blue, Green, Blue, Green)
      val guessWithoutFullMatches = guess
      val master = List(Green, Blue, Green, Blue)
    }
    
    test("2b: all partial matches") {
      new Move2b {
        assert(removeFullMatches(guess, master) === List(Blue, Green, Blue, Green).reverse) // TODO: sort lists!
        assert(countFullMatches(guess, master) === 0)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List())
        assert(countPartialMatches(guess, master) === 4)
        assert(isCorrect(guess, master) == false)
      }
    }
    
    //some full matches, some partial
    trait Move3a extends MastermindDef {
      val guess = List(Blue, Blue, Green, Orange)
      val guessWithoutFullMatches = List(Green, Orange)
      val master = List(Blue, Blue, Orange, Purple)
    }
    
    test("3a: some full matches, some partial") {
      new Move3a {
        assert(removeFullMatches(guess, master) === List(Green, Orange).reverse) // TODO: sort lists!
        assert(countFullMatches(guess, master) === 2)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List(Green))
        assert(countPartialMatches(guess, master) === 1)
        assert(isCorrect(guess, master) == false)
      }
    }
    
    trait Move3b extends MastermindDef {
      val guess = List(Blue, Yellow, Purple, Red)
      val guessWithoutFullMatches = List(Blue, Purple)
      val master = List(Purple, Yellow, Orange, Red)
    }
    
    test("3b: some full matches, some partial") {
      new Move3b {
        assert(removeFullMatches(guess, master) === List(Blue, Purple).reverse) // TODO: sort lists!
        assert(countFullMatches(guess, master) === 2)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List(Blue))
        assert(countPartialMatches(guess, master) === 1)
        assert(isCorrect(guess, master) == false)
      }
    }
    
    // no matches
    trait Move4a extends MastermindDef {
      val guess = List(Blue, Blue, Green, Orange)
      val guessWithoutFullMatches = List(Blue, Blue, Green, Orange)
      val master = List(Red, Purple, Purple, Yellow)
    }
    
    test("4a: no matches") {
      new Move4a {
        assert(removeFullMatches(guess, master) === List(Blue, Blue, Green, Orange).reverse) // TODO: sort lists!
        assert(countFullMatches(guess, master) === 0)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List(Blue, Blue, Green, Orange).reverse)
        assert(countPartialMatches(guess, master) === 0)
        assert(isCorrect(guess, master) == false)
      }
    }
    
//    test("5a: guesses and results") {
//      assert(Guess())
//    }
}