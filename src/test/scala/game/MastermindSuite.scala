package test.scala.game

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import main.scala.game.MastermindDef

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
        assert(removeFullMatches(guess, master) === List()) // TODO: sort lists!
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
        assert(removeFullMatches(guess, master) === List(Magenta, Magenta, Blue, Green).reverse) // TODO: sort lists!
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
      val guess = List(Blue, Blue, Green, Magenta)
      val guessWithoutFullMatches = List(Green, Magenta)
      val master = List(Blue, Blue, Magenta, Cyan)
    }
    
    test("3a: some full matches, some partial") {
      new Move3a {
        assert(removeFullMatches(guess, master) === List(Green, Magenta).reverse) // TODO: sort lists!
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
        assert(removeFullMatches(guess, master) === List(Blue, Cyan).reverse) // TODO: sort lists!
        assert(countFullMatches(guess, master) === 2)
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
        assert(removeFullMatches(guess, master) === List(Blue, Blue, Green, Magenta).reverse) // TODO: sort lists!
        assert(countFullMatches(guess, master) === 0)
        assert(removePartialMatches(guessWithoutFullMatches, master) === List(Blue, Blue, Green, Magenta).reverse)
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
        

//        val num = readLine("enter an int> ")
//         Console.out.println(num)  
//         
//        Console.out.println( "Test " + Console.BLUE + aChar)
//        Console.out.println( "Test " + Console.YELLOW + aChar)
//        Console.out.println( "Test " + Console.GREEN + aChar)
//        Console.out.println( "Test " + Console.Cyan + aChar)
//        Console.out.println( "Test " + Console.Magenta + aChar + Console.RESET)
        }
    }

}