import Chapter3_DataStructures.{MyList, MyNil}
import org.scalatest.FlatSpec

/**
  * Created by rweinstein on 8/7/17.
  */
class Chapter3Tests extends FlatSpec {

  "The answer to exercise 3.1" should "equal 3" in {
    assert(MyList.x == 3)
  }

  "The method tail" should "make a new list of all elements except for the head" in {
    val originalList = MyList(1,2,3)
    val listTail = MyList.tail(originalList)
    assert(listTail == MyList(2,3))
  }

  "The method tail" should "return MyNil if the list is of type MyNil" in {
    val originalList = MyNil
    val listTail = MyList.tail(originalList)
    assert(listTail == MyNil)
  }

  "The method setHead" should "make a new list with the head element replaced by the given value" in {
    val originalList = MyList(1,2,3)
    val listWithDifferentHead = MyList.setHead(originalList, 4)
    assert(listWithDifferentHead == MyList(4,2,3))
  }

  "The method setHead" should "throw an error if trying to replace the head of a Nil MyList" in {
    val originalList = MyNil
    val errorList = intercept[Exception] {
      MyList.setHead(originalList, 4)
    }
    assert(errorList.getMessage == "Cannot replace head of Nil MyList")
  }

  "The method drop" should "make a new list without the desired number of dropped elements from the head" in {
    val originalList = MyList(1,2,3,4,5,6,7,8,9,10)
    val droppedList = MyList.drop(originalList, 5)
    assert(droppedList == MyList(6,7,8,9,10))
  }

  "The method drop" should "return the same list if given a drop value of <= 0" in {
    val originalList = MyList(1,2,3,4,5,6,7,8,9,10)
    val droppedList = MyList.drop(originalList, -3)
    assert(droppedList == MyList(1,2,3,4,5,6,7,8,9,10))
  }

  "The method drop" should "return MyNil when given a list of type MyNil" in {
    val originalList = MyNil
    val droppedList = MyList.drop(originalList, 5)
    assert(droppedList == MyNil)
  }

}
