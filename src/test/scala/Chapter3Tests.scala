import Chapter3_DataStructures.MyList
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

}
