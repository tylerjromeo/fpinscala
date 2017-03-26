package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = flatMap(e => Right(f(e)))

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(l) => Left(l)
   case Right(r) => f(r)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(r) => Right(r)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   flatMap(a => b.map(f(a, _)))
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = ???

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def main(args: Array[String]): Unit = {
    test("map", Seq(
      (Right(2), Right(1).map(_ + 1)),
      (Left(1), Left(1).asInstanceOf[Either[Int, Int]].map(_ + 1))
    ))
    test("flatMap", Seq(
      (Right(2), Right(1).flatMap(x => Right(x + 1))),
      (Left(1), Left(1).asInstanceOf[Either[Int, Int]].flatMap(x => Right(x + 1))),
      (Left("bap"), Right(1).flatMap(_ => Left("bap")))
    ))
    test("orElse", Seq(
      (Right(1), Right(1).orElse(Left(2))),
      (Right(2), Left(1).orElse(Right(2))),
      (Left(2), Left(1).orElse(Left(2)))
    ))
    test("map2", Seq(
      (Right(5), Right(1).map2(Right(4))(_ + _)),
      (Left("first"), Left("first").asInstanceOf[Either[String, Int]].map2(Right(4))(_ + _)),
      (Left("second"), Right(1).map2(Left("second").asInstanceOf[Either[String, Int]])(_ + _)),
      (Left("first"), Left("first").asInstanceOf[Either[String, Int]].map2(Left("second").asInstanceOf[Either[String, Int]])(_ + _))
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