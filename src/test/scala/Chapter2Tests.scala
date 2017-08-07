import Chapter2_GettingStarted.Chapter2
import org.scalatest.FlatSpec

/**
  * Created by rweinstein on 8/7/17.
  */
class Chapter2Tests extends FlatSpec {

  "The 10th Fibonacci number" should "equal 55" in {
    val tenthFibNumber = Chapter2.getNthFibonacciNum(10)
    assert(tenthFibNumber == 55)
  }

  "The -101st Fibonacci number" should "equal -1 because it's a bad number" in {
    val negativeFirst = Chapter2.getNthFibonacciNum(-101)
    assert(negativeFirst == -1)
  }

  "The 50th Fibonacci number" should "equal 12586269025" in {
    val fiftiethFibNumber: BigInt = Chapter2.getNthFibonacciNum(50)
    assert(fiftiethFibNumber == 12586269025L)
  }

  "The unsorted array" should "equal false when it's looking for an array sorted in ascending order" in {
    val unsortedArray = Array(5,2,54,1,76,2)
    assert(!Chapter2.isSorted(unsortedArray, (a: Int, b: Int) => a < b))
  }

  "The sorted array" should "equal true because it's looking for an array sorted in ascending order" in {
    val sortedArray = Array(5,7,14,31,76,200)
    assert(Chapter2.isSorted(sortedArray, (a: Int, b: Int) => a < b))
  }

  "This method" should "become curried" in {
    val originalMethod = (a: Int, b: Int) => a < b
    val curriedLessThanMethod = Chapter2.curry(originalMethod)
    assert(curriedLessThanMethod(5)(10))
  }

  "This method" should "become uncurried" in {
    val originalMethod = (a: Int, b: Int) => a < b
    val curriedLessThanMethod = Chapter2.curry(originalMethod)
    assert(curriedLessThanMethod(5)(10))
    val uncurriedLessThanMethod = Chapter2.uncurry(curriedLessThanMethod)
    assert(uncurriedLessThanMethod(5,10))
  }

  "This method" should "compose the two supplied to it" in {
    val timesTwo = (x: Int) => x * 2
    val dividedByTwo = (x: Int) => x / 2
    val composed = Chapter2.compose(timesTwo, dividedByTwo)
    assert(composed(2) == 2)
  }
}
