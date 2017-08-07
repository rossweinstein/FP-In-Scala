package Chapter3_DataStructures

import scala.annotation.tailrec

sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class MyConstruct[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil => 0
    case MyConstruct(head, tail) => head + sum(tail)
  }

  def product(values: MyList[Double]): Double = values match {
    case MyNil => 1.0
    case MyConstruct(0.0, _) => 0.0
    case MyConstruct(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): MyList[A] =
  if (as.isEmpty) MyNil
  else MyConstruct(as.head, apply(as.tail: _*))

  // exercise 3.1
  // answer: case MyConstruct(head1, MyConstruct(head2, MyConstruct(3, MyConstruct(4, _)))) => head1 + head2
  val x = MyList(1,2,3,4,5) match {
    case MyConstruct(head, MyConstruct(4, _)) => head
    case MyNil => 42
    case MyConstruct(head1, MyConstruct(head2, MyConstruct(3, MyConstruct(4, _)))) => head1 + head2
    case MyConstruct(head, tail) => head + sum(tail)
    case _ => 101
  }

  // exercise 3.2: write a function that retails all elements except for the head
  def tail[A](theList: MyList[A]): MyList[A] = theList match {
    case MyNil => MyNil
    case MyConstruct(_, tail) => tail
  }

  // exercise 3.3: write a function that replaces the head of the list with a new value
  def setHead[A](theList: MyList[A], replacementHead: A): MyList[A] = theList match {
    case MyNil => sys.error("Cannot replace head of Nil MyList")
    case MyConstruct(_, tail) => MyConstruct(replacementHead, tail)
  }

  // exercise 3.4: write a generalized tail function to drop n number of elements
  def drop[A](theList: MyList[A], elementsToDrop: Int): MyList[A] = {
    @tailrec
    def getRidOfElements(list: MyList[A], droppedElements: Int = 1): MyList[A] = list match {
      case MyNil => MyNil
      case MyConstruct(_, tail) => if (droppedElements == elementsToDrop) tail else getRidOfElements(tail, droppedElements + 1)
    }
    if (elementsToDrop <= 0) theList else getRidOfElements(theList)
  }
}
