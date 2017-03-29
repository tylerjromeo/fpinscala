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
    foldRight(true)((a, z) => p(a) && z)
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A]) {
      case (a, z) if p(a) => Stream.cons(a, z)
      case (_, _) => Empty
    }
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, z) => Stream.cons(f(a), z))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]) {
      case (a, z) if f(a) => Stream.cons(a, z)
      case (_, z) => z
    }
  }

  def append[B >: A](t: => Stream[B]): Stream[B] = {
    foldRight(t)((a, z) => Stream.cons(a, z))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, z) => f(a).append(z))
  }

  def map2[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }
  }

  def take2(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None
    }
  }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Empty) => None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(_._2.isDefined).forAll((pair) => {
      for {
        a <- pair._1
        b <- pair._2
      } yield a == b
    }.getOrElse(false))
  }

  def tails(): Stream[Stream[A]] = {
    Stream.unfold(this){
      case Cons(h, t) => Some(Cons(h, t), t())
      case Empty => None
    } append Stream(Stream.empty)
  }
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

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  def fibs(): Stream[Int] = {
    def next(prev2: Int, prev1: Int): Stream[Int] = Stream.cons(prev1, next(prev1, prev2 + prev1))
    cons(0, next(0, 1))
  }

  val ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def fibs2(): Stream[Int] = unfold((0, 1): (Int, Int))((ns) => Some(ns._1, (ns._2, ns._1 + ns._2)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

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
        assert(false);
        5
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
        assert(false);
        2
      }).headOption.map(_.apply()))
    ))
    test("map", Seq(
      (Stream(2, 3, 4).toList, Stream(1, 2, 3).map(_ + 1).toList),
      (Stream.empty.toList, Stream.empty[Int].map(_ + 1).toList),
      (Stream(1, 2).toList, Stream(1, 2, 3).map(x => if (x == 3) assert(false) else x).take(2).toList)
    ))
    test("filter", Seq(
      (Stream(2).toList, Stream(1, 2, 3).filter(_ == 2).toList),
      (Stream(1, 2, 3).toList, Stream(1, 2, 3).filter((_) => true).toList),
      (empty.toList, Stream(1, 2, 3).filter((_) => false).toList),
      (empty.toList, empty[Int].filter(_ == 2).toList)
    ))
    test("append", Seq(
      (Stream(1, 2, 3, 4).toList, Stream(1, 2).append(Stream(3, 4)).toList),
      (Stream(1, 2).toList, Stream(1, 2).append(empty).toList),
      (Stream(1, 2).toList, empty.append(Stream(1, 2)).toList)
    ))
    test("flatmap", Seq(
      (Stream(1, 1, 2, 2, 3, 3).toList, Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList),
      (empty.toList, empty[Int].flatMap(x => Stream(x, x)).toList)
    ))
    test("constant", Seq(
      (Some("a"), constant("a").headOption),
      (true, constant("a").map(s => s + s).exists(_ == "aa")),
      (false, constant(7).forAll(_ != 7)),
      (Stream(1, 1, 1, 1).toList, constant(1).take(4).toList)
    ))
    test("from", Seq(
      (Some(1), from(1).headOption),
      (true, from(1).exists(_ == 6)),
      (Stream(-2, 0, 2, 4, 6, 8).toList, from(-1).map(_ * 2).take(6).toList),
      (false, from(1).forAll(_ < 30))
    ))
    test("fibs", Seq(
      (Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55).toList, Stream.fibs().take(11).toList)
    ))
    test("unfold", Seq(
      (Stream(1, 2, 3, 4).toList, unfold(0)(x => Some((x + 1, x + 1))).take(4).toList),
      (Stream("2", "4", "8", "16").toList, unfold(1)(x => Some(((x + x).toString, x + x))).take(4).toList)
    ))
    test("ones2", Seq(
      (Some(1), ones2.headOption),
      (true, ones2.map(x => x.toString + x.toString).exists(_ == "11")),
      (false, ones2.forAll(_ != 1)),
      (Stream(1, 1, 1, 1).toList, ones2.take(4).toList)
    ))
    test("constant2", Seq(
      (Some("a"), constant2("a").headOption),
      (true, constant2("a").map(s => s + s).exists(_ == "aa")),
      (false, constant2(7).forAll(_ != 7)),
      (Stream(1, 1, 1, 1).toList, constant2(1).take(4).toList)
    ))
    test("from2", Seq(
      (Some(1), from2(1).headOption),
      (true, from2(1).exists(_ == 6)),
      (Stream(-2, 0, 2, 4, 6, 8).toList, from2(-1).map(_ * 2).take(6).toList),
      (false, from2(1).forAll(_ < 5))
    ))
    test("fibs2", Seq(
      (Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55).toList, Stream.fibs2().take(11).toList)
    ))

    test("map2", Seq(
      (Stream(2, 3, 4).toList, Stream(1, 2, 3).map2(_ + 1).toList),
      (Stream.empty.toList, Stream.empty[Int].map2(_ + 1).toList),
      // This fails because it evaluates 1 further than the take...
      //      (Stream(1, 2).toList, Stream(1, 2, 3, 4).map2(x => if (x == 3) assert(false) else x).take(2).toList),
      (Stream(1, 2).toList, Stream(1, 2, 3, 4).map2(x => if (x == 4) assert(false) else x).take(2).toList)
    ))
    test("take2", Seq(
      (Stream(1, 2).toList, Stream(1, 2, 3, 4, 5).take2(2).toList),
      (Stream(1, 2, 3).toList, Stream(1, 2, 3).take2(5).toList),
      (Nil, Stream(1, 2, 3).take2(0).toList)
    ))
    test("takeWhile3", Seq(
      (Stream(1, 2, 3).toList, Stream(1, 2, 3, 4, 5).takeWhile3(_ < 4).toList),
      (Nil, Stream(1, 2, 3).takeWhile3(_ < 1).toList),
      (Stream(1, 2, 3).toList, Stream(1, 2, 3, 4, 1, 2, 3).takeWhile3(_ < 4).toList)
    ))
    test("zipWith", Seq(
      (Stream("a1", "b2", "c3", "d4").toList, Stream("a", "b", "c", "d").zipWith(Stream.from(1))((s, x) => s + x.toString).toList)
    ))
    test("zipAll", Seq(
      (Stream((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6))).toList, Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).toList),
      (Stream((Some(1), None), (Some(2), None), (Some(3), None)).toList, Stream(1, 2, 3).zipAll(empty).toList),
      (Stream((Some(1), Some(4)), (Some(2), Some(5)), (None, Some(6))).toList, Stream(1, 2).zipAll(Stream(4, 5, 6)).toList)
    ))
    test("startsWith", Seq(
      (true, Stream(1, 2, 3).startsWith(Stream(1, 2))),
      (true, Stream(1, 2, 3).startsWith(Stream(1, 2, 3))),
      (false, Stream(1, 2).startsWith(Stream(1, 2, 3))),
      (false, Stream(1, 2, 3).startsWith(Stream(1, 2, 2)))
    ))
    test("tails", Seq(
      (Stream(empty).map(_.toList).toList, empty.tails().map(_.toList).toList),
      (Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), empty).map(_.toList).toList, Stream(1, 2, 3).tails().map(_.toList).toList)
    ))
  }
}