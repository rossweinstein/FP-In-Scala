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

  def sum2(ints: MyList[Int]) = foldRight(ints,0)((x,y) => x + y)

  def product(values: MyList[Double]): Double = values match {
    case MyNil => 1.0
    case MyConstruct(0.0, _) => 0.0
    case MyConstruct(head, tail) => head * product(tail)
  }

  def product2(values: MyList[Double]) = foldRight(values, 1.0)(_ * _)

  def foldRight[A, B](theList: MyList[A], seed: B)(func: (A,B) => B): B = theList match {
    case MyNil => seed
    case MyConstruct(head, tail) => func(head, foldRight(tail, seed)(func))
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

  // exercise 3.5: write a function which removes elements from a MyList if they match a predicate
  def dropWhile[A](theList: MyList[A], conditionFunc: A => Boolean): MyList[A] = {
    @tailrec
    def ridMatchingValues(list: MyList[A], accum: MyList[A]): MyList[A] = list match {
      case MyNil => reverse(accum)
      case MyConstruct(head, tail) =>
        val constructedList = if (conditionFunc(head)) accum else MyConstruct(head, accum)
        ridMatchingValues(tail, constructedList)
    }
    if (theList == MyNil) MyNil else ridMatchingValues(theList, MyList[A]())
  }

  // not apart of the exercises, needed it for how my loops are constructed
  def reverse[A](theList: MyList[A]): MyList[A] = {
    @tailrec
    def reversal(originalList: MyList[A], reversedList: MyList[A]): MyList[A] = originalList match {
      case MyNil => reversedList
      case MyConstruct(head, tail) => reversal(tail, MyConstruct(head, reversedList))
    }
    if (theList == MyNil) MyNil else reversal(theList, MyList[A]())
  }

  // exercise 3.6: write a function that returns a MyList consisting of all but the last element
  def init[A](theList: MyList[A]): MyList[A] = {
    @tailrec
    def allButTheLast(list: MyList[A], accum: MyList[A]): MyList[A] = list match {
      case MyConstruct(_, t) if (t == MyNil) => reverse(accum)
      case MyConstruct(head, tail) => allButTheLast(tail, MyConstruct(head, accum))
    }
    if (theList == MyNil) MyNil else allButTheLast(theList, MyList[A]())
  }

  // exercise 3.7: can product, implemented using foldRight, halt if it encounters 0.0>
  // answer: Maybe, at the moment, though, I cannot think of a way to do it

  // exercise 3.8: what happens when you pass MyNil or MyConstruct to foldRight?
  // answer: the original list is returned (see tests for results)

  // exercise 3.9: compute length of a list using foldRight
  def length[A](theList: MyList[A]): Int = {
    if (theList == MyNil) 0 else foldRight(theList, 0)((_, total) => total + 1)
  }
}
