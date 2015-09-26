package test.scala.game

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import main.scala.game.MastermindDef
//import main.scala.game.MastermindDef.UnknownColorException
import main.scala.game.MastermindGame

@RunWith(classOf[JUnitRunner])
class MastermindSuite extends FunSuite {
      
    // all full matches
     trait Move1a extends MastermindDef {
      val guess = List(Magenta, Magenta, Magenta, Magenta)
      val guessWithoutFullMatches = List()
      val master = List(Magenta, Magenta, Magenta, Magenta)
    }
    
    test("1a: all full matches") {
      new Move1a {      
        assert(removeFullMatches(guess, master) === (Nil, Nil))
        assert(countFullMatches(guess, master) === 4)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List())
        assert(countPartialMatches(guess, master) === 0)
        assert(isCorrect(guess, master) == true)
      }
    }
    
    // all partial matches
    trait Move2a extends MastermindDef {
      val guess = List(Magenta, Magenta, Blue, Green)
      val guessWithoutFullMatches = guess
      val master = List(Blue, Green, Magenta, Magenta)
    }
    
    test("2a: all partial matches") {
      new Move2a {
        assert(removeFullMatches(guess, master) === (List(Magenta, Magenta, Blue, Green), master))
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
        assert(removeFullMatches(guess, master) === (List(Blue, Green, Blue, Green), master))
        assert(countFullMatches(guess, master) === 0)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List())
        assert(countPartialMatches(guess, master) === 4)
        assert(isCorrect(guess, master) == false)
      }
    }
    
    //some full matches, some partial
    trait Move3a extends MastermindDef {
      val guess = List(Blue, Blue, Green, Magenta)
      val guessWithoutFullMatches = List(Green, Magenta)
      val master = List(Blue, Blue, Magenta, Cyan)
    }
    
    test("3a: some full matches, some partial") {
      new Move3a {
        assert(removeFullMatches(guess, master) === (List(Green, Magenta), List(Magenta, Cyan)))
        assert(countFullMatches(guess, master) === 2)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List(Green))
        assert(countPartialMatches(guess, master) === 1)
        assert(isCorrect(guess, master) == false)
      }
    }
    
    trait Move3b extends MastermindDef {
      val guess = List(Blue, Yellow, Cyan, Red)
      val guessWithoutFullMatches = List(Blue, Cyan)
      val master = List(Cyan, Yellow, Magenta, Red)
    }
    
    test("3b: some full matches, some partial") {
      new Move3b {
        assert(removeFullMatches(guess, master) === (List(Blue, Cyan), List(Cyan, Magenta)))
        assert(countFullMatches(guess, master) === 2)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List(Blue))
        assert(countPartialMatches(guess, master) === 1)
        assert(isCorrect(guess, master) == false)
      }
    }

    trait Move3c extends MastermindDef {
      val guess = List(Blue, Blue, Blue, Magenta)
      val guessWithoutFullMatches = List(Blue, Blue, Magenta)
      val master = List(Blue, Red, Magenta, Red)
    }
    
    test("3c: some full matches, some partial") {
      new Move3c {
        assert(removeFullMatches(guess, master) === (List(Blue, Blue, Magenta), List(Red, Magenta, Red)))
        assert(countFullMatches(guess, master) === 1)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List(Blue))
        assert(countPartialMatches(guess, master) === 1)
        assert(isCorrect(guess, master) == false)
      }
    }
    
    // no matches
    trait Move4a extends MastermindDef {
      val guess = List(Blue, Blue, Green, Magenta)
      val guessWithoutFullMatches = List(Blue, Blue, Green, Magenta)
      val master = List(Red, Cyan, Cyan, Yellow)
    }
    
    test("4a: no matches") {
      new Move4a {
        assert(removeFullMatches(guess, master) === (List(Blue, Blue, Green, Magenta), master))
        assert(countFullMatches(guess, master) === 0)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List(Blue, Blue, Green, Magenta))
        assert(countPartialMatches(guess, master) === 0)
        assert(isCorrect(guess, master) == false)
      }
    }
    
    trait Move5a extends MastermindDef {
      def testGenerateResult(len: Int, numColors: Int): Unit = {
        val result = generateResult(len, numColors)
        val colorList = getPotentialColorList(numColors)
        println("result of size " + len + " with " + numColors + " possible colors: " + result + ". \n Using potential color list:" + colorList)
        // ensure size of result is correct
        if (numColors != 0) {
          assert(result.length === len)
          if (len != 0) {
            assert((result.toSet & colorList.toSet).size >= 1)
            assert((result.toSet & GUESS_COLORS.drop(numColors).toSet).size === 0)        
          }
        } else {
          assert(result.length === 0)
        }       
      }    
    }
    
    test("5a: generate results") {
      new Move5a {
        testGenerateResult(0, 5)
        testGenerateResult(3, 1)
        testGenerateResult(2, 2)
        testGenerateResult(5, 1)
        testGenerateResult(5, 2)
        testGenerateResult(4, 6)
        testGenerateResult(8, 6)
        val asciiNum = 79
        val aChar = asciiNum.toChar
        Console.out.println("Test " + " " + Console.RED + aChar + " " + Console.BLUE + aChar + " " + Console.YELLOW + aChar + " " 
            + Console.GREEN + aChar + " " + Console.CYAN + aChar + " " + Console.MAGENTA + aChar
            + Console.WHITE + " |" + Console.BOLD + Console.BLACK + " " + aChar + Console.WHITE + " " + aChar
            + Console.RESET)
        }
    }

    // no matches
    test("6a: disallowed colors") {
      new MastermindDef {
        intercept[Exception] {
          getColor("yellow", 3)
        }
      }
    }
    
    test("6b: allowed colors") {
      new MastermindDef {
        try {
          getColor("green", 3)
        } catch {
          case _ : Throwable => fail("green should be allowed")
        }        
      }
    }

}