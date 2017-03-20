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

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, n) => n + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Int]): Int = foldLeft(ns, 1)(_ * _)

  def length2(ns: List[Int]): Int = foldLeft(ns, 0)((count, _) => count + 1)

  def reverse[A](ns: List[A]): List[A] = foldLeft(ns, Nil: List[A])((acc, x) => Cons(x, acc))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((b, a) => f(a, b))

  def append2[A](l: List[A], l2: List[A]): List[A] = foldRight(l, l2)(Cons(_, _))

  def flatten[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, Nil: List[A])(append2)
  }

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
    test("length", Seq(
      (3, length(List(1, 2,3))),
      (0, length(Nil)),
      (1, length(List(1)))
    ))
    test("foldLeft", Seq(
      (6, foldLeft(List(1, 2, 3), 0)(_ + _)),
      (-6, foldLeft(List(1, 2, 3), 0)(_ - _)),
      (24, foldLeft(List(1, 2, 3, 4), 1)(_ * _)),
      (0, foldLeft(Nil, 0)((x,y) => {assert(false); x}))
    ))
    test("sum3", Seq(
      (6, sum3(List(1, 2,3))),
      (0, sum3(Nil)),
      (1, sum3(List(1)))
    ))
    test("product3", Seq(
      (24, product3(List(1, 2,3, 4))),
      (1, product3(Nil)),
      (1, product3(List(1)))
    ))
    test("length2", Seq(
      (3, length2(List(1, 2,3))),
      (0, length2(Nil)),
      (1, length2(List(1)))
    ))
    test("reverse", Seq(
      (List(3,2,1), reverse(List(1, 2,3))),
      (Nil, reverse(Nil)),
      (List(1), reverse(List(1)))
    ))
    test("foldLeft2", Seq(
      (6, foldLeft2(List(1, 2, 3), 0)(_ + _)),
      (-6, foldLeft2(List(1, 2, 3), 0)(_ - _)),
      (24, foldLeft2(List(1, 2, 3, 4), 1)(_ * _)),
      (0, foldLeft2(Nil, 0)((x,y) => {assert(false); x}))
    ))
    test("foldRight2", Seq(
      (6, foldRight2(List(1, 2, 3), 0)(_ + _)),
      (2, foldRight2(List(1, 2, 3), 0)(_ - _)),
      (24, foldRight2(List(1, 2, 3, 4), 1)(_ * _)),
      (0, foldRight2(Nil, 0)((x,y) => {assert(false); x}))
    ))
    test("append2", Seq(
      (List(1,2,3,4), append2(List(1,2), List(3,4))),
      (Nil, append2(Nil, Nil)),
      (List(1), append2(List(1), Nil)),
      (List(1), append2(Nil, List(1)))
    ))
    test("flatten", Seq(
      (List(1,2,3,4), flatten(List(List(1,2), List(3), List(4)))),
      (Nil, flatten(List(Nil, Nil))),
      (List(1), flatten(List(Nil, List(1), Nil))),
      (List(1, 2, 3), flatten(List(Nil, List(1), Nil, List(2,3))))
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
