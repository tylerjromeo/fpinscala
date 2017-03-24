package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _}
// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(x => if (f(x)) Some(x) else None)
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap{ m =>
      mean(xs.map{ x =>
        math.pow(x - m, 2)
      })
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

  def main(args: Array[String]): Unit = {
    test("map", Seq(
      (Some("2"), Some(1).map(x => (x + 1).toString)),
      (None, None.map((_) => assert(false)))
    ))
    test("flatMap", Seq(
      (Some(2), Some(1).flatMap(x => Some(x + 1))),
      (None, Some(1).flatMap(_ => None)),
      (None, None.map((_) => assert(false)))
    ))
    test("getOrElse", Seq(
      (1, Some(1).getOrElse({
        assert(false); "BAD"
      })),
      ("GOOD", None.getOrElse("GOOD")),
      (Some(1), None.getOrElse(Some(1)))
    ))
    test("orElse", Seq(
      (Some(1), Some(1).orElse({
        assert(false); Some(33)
      })),
      (Some("GOOD"), None.orElse(Some("GOOD")))
    ))
    test("filter", Seq(
      (Some(1), Some(1).filter(_ == 1)),
      (None, Some(1).filter(_ == 0)),
      (None, None.filter(_ == 0))
    ))

    test("variance", Seq(
      (Some(2.0815999999999995), variance(Seq(1.0, 2.0, 3.0, 4.0, 5.1))),
      (None, variance(Seq()))
    ))

    test("map2", Seq(
      (Some(3), map2(Some(1), Some(2))(_ + _)),
      (None, map2(None: Option[Int], Some(2))(_ + _)),
      (None, map2(Some(1), None: Option[Int])(_ + _))
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