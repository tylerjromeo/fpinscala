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

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen[B](sample = sample.flatMap(f(_).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(Gen.listOfN(_, Gen.this))
  }
}


object Gen {
  def unit[A](a: => A): Gen[A] = Gen[A](
    State.unit(a)
  )

  def boolean: Gen[Boolean] = Gen[Boolean](
    State[RNG, Int](RNG.int).map(_ % 2 == 0)
  )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen[List[A]](
    State.sequence(List.fill(n)(g.sample))
  )

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen[Int](sample =
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

