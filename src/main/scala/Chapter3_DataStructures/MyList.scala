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
}
