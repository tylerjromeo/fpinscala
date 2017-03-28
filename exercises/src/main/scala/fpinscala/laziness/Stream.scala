package fpinscala.laziness

sealed trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil: List[A]
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) Cons(h, () => t().take(n - 1)) else Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else Cons(h, t)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case Cons(_, _) => Empty
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true) {
      case (a, z) => z && p(a)
    }
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]) {
      case (a, z) if p(a) => Stream.cons(a, z)
      case (_, _) => Empty
    }
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A]) {
      case (a, _) => Some(a)
    }
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B]) {
      case (a, z) => Stream.cons(f(a), z)
    }
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) {
      case (a, z) if f(a) => Stream.cons(a, z)
      case (_, z) => z
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  def test[A, B](name: String, ioTable: Seq[(A, B)]): Unit = {
    println(s"$name test:")
    ioTable.foreach {
      case (expected, result) if expected != result => println(s"expected $expected but was $result")
      case _ => ()
    }
    println(s"$name test complete\n")
  }

  def main(args: Array[String]): Unit = {
    test("toList", Seq(
      (List(1, 2, 3), Stream(1, 2, 3).toList)
    ))
    test("take", Seq(
      (Stream(1, 2).toList, Stream(1, 2, 3, 4, 5).take(2).toList),
      (Stream(1, 2, 3).toList, Stream(1, 2, 3).take(5).toList),
      (Nil, Stream(1, 2, 3).take(0).toList)
    ))
    test("drop", Seq(
      (Stream(3, 4, 5).toList, Stream(1, 2, 3, 4, 5).drop(2).toList),
      (Nil, Stream(1, 2, 3).drop(5).toList),
      (Stream(1, 2, 3).toList, Stream(1, 2, 3).drop(0).toList)
    ))
    test("takeWhile", Seq(
      (Stream(1, 2, 3).toList, Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList),
      (Nil, Stream(1, 2, 3).takeWhile(_ < 1).toList),
      (Stream(1, 2, 3).toList, Stream(1, 2, 3, 4, 1, 2, 3).takeWhile(_ < 4).toList)
    ))
    test("forAll", Seq(
      (true, Stream(3, 4, 5).forAll(_ > 2)),
      (false, Stream(3, 4, 5).forAll(_ < 4)),
      (true, empty.asInstanceOf[Stream[Int]].forAll(_ < 4)),
      (false, scala.collection.immutable.Stream(() => 3, () => 4, () => {
        assert(false); 5
      }).forall(f => f() < 3))
    ))
    test("takeWhile2", Seq(
      (Stream(1, 2, 3).toList, Stream(1, 2, 3, 4, 5).takeWhile2(_ < 4).toList),
      (Nil, Stream(1, 2, 3).takeWhile2(_ < 1).toList),
      (Stream(1, 2, 3).toList, Stream(1, 2, 3, 4, 1, 2, 3).takeWhile2(_ < 4).toList)
    ))
    test("headOption", Seq(
      (Some(1), Stream(1, 2, 3).headOption),
      (None, Stream.empty.headOption),
      (Some(1), Stream(() => 1, () => {
        assert(false); 2
      }).headOption.map(_.apply()))
    ))
    test("map", Seq(
      (Stream(2, 3, 4).toList, Stream(1, 2, 3).map(_ + 1).toList),
      (Stream.empty.toList, Stream.empty[Int].map(_ + 1).toList),
      (Stream(1, 2).toList, Stream(1, 2, 3).map(x => if (x == 3) assert(false) else x).take(2).toList)
    ))
    test("filter", Seq(
      (Stream(2).toList, Stream(1,2,3).filter(_ == 2).toList),
      (Stream(1,2,3).toList, Stream(1,2,3).filter((_) => true).toList),
      (empty.toList, Stream(1,2,3).filter((_) => false).toList),
      (empty.toList, empty[Int].filter(_ == 2).toList)
    ))
  }
}