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

  // generates a Double in the ranfe [0, 1)
  def double: Gen[Double] = Gen[Double](
    State[RNG, Double](RNG.doubleViaMap)
  )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen[List[A]](
    State.sequence(List.fill(n)(g.sample))
  )

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen[Int](sample =
    State[RNG, Int](RNG.map[Int, Int](RNG.nonNegativeLessThan(stopExclusive - start))(_ + start))
  )

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(if(_) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val total = g1._2 + g2._2
    val prob1 = g1._2 / total
    double.flatMap(d => if(d < prob1) g1._1 else g2._1)
  }
}

//trait Gen[A] {
//  def map[A, B](f: A => B): Gen[B] = ???
//
//  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

