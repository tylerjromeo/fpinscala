package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, xs) => xs
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, xs) => Cons(h, xs)
    case Nil => Cons(h, Nil)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case ll if n <= 0 => ll
    case Nil => Nil
    case Cons(_, t) => drop(t, n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case ll => ll
  }

  //not tail recursive, and has to go through the entire list. poopy. (but it's all part of the lesson)
  def init[A](l: List[A]): List[A] =  l match {
    case Nil => Nil
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???

  def main(args: Array[String]): Unit = {
    test("tail", Seq(
      (List(2,3), tail(List(1, 2,3))),
      (Nil, tail(Nil)),
      (Nil, tail(List(1)))
    ))
    test("setHead", Seq(
      (List(5, 2,3), setHead(List(1, 2,3), 5)),
      (List("bip"), setHead(Nil, "bip")),
      (List(List(1), List("a"), Nil), setHead(List(Nil, List("a"), Nil), List(1)))
    ))
    test("drop", Seq(
      (List(3), drop(List(1, 2, 3), 2)),
      (Nil, drop(List(1,2,3,4,5), 5)),
      (List(1), drop(List(1), 0))
    ))
    test("dropWhile", Seq(
      (List(3), dropWhile(List(1, 2, 3), (n: Int) => n < 3)),
      (List(3, 1), dropWhile(List(1, 2, 3, 1), (n: Int) => n < 3)),
      (Nil, dropWhile(List(1, 2, 3, 1), (n: Int) => n < 4)),
      (Nil, dropWhile(Nil, (_: Int) => {assert(false); true})),
      (List(1000, 1, 1, 1, 1), dropWhile(List(1000, 1, 1, 1, 1), (n: Int) => n < 3))
    ))
    test("init", Seq(
      (List(1,2), init(List(1, 2,3))),
      (Nil, init(Nil)),
      (Nil, init(List(1)))
    ))
  }

  def test[A, B](name: String, ioTable: Seq[(A, B)]): Unit = {
    println(s"$name test:")
    ioTable.foreach {
      case (expected, result) if expected != result => println(s"expected $expected but was $result")
      case _ => ()
    }
    println(s"$name test complete\n")
  }
}
