package fpinscala.testing

import fpinscala.state.{RNG, State}
import fpinscala.testing.Prop.{FailedCase, SuccessCount}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???

//  def &&(p: Prop): Prop = new Prop {
//    override def check: Boolean = Prop.this.check && p.check
//  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A])


object Gen {
  def unit[A](a: => A): Gen[A] = ???
  def choose(start: Int, stopExclusive:Int): Gen[Int] = Gen[Int](sample =
    State[RNG, Int](RNG.map[Int, Int](RNG.nonNegativeLessThan(stopExclusive - start))(_ + start))
  )
}

//trait Gen[A] {
//  def map[A, B](f: A => B): Gen[B] = ???
//
//  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

