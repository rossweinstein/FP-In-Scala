package Chapter3_DataStructures

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

  // exercise 3.2
  def tail[A](theList: MyList[A]): MyList[A] = theList match {
    case MyNil => MyNil
    case MyConstruct(_, tail) => tail
  }
}
