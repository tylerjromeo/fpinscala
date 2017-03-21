package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //not tail recursive
  def size(t: Tree[_]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  //not tail recursive
  def max(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => max(l).max(max(r))
  }

  //not tail recursive
  def depth(t: Tree[_]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + depth(l).max(depth(r))
  }

  def main(args: Array[String]): Unit = {
    test("size", Seq(
      (1, size(Leaf(0))),
      (3, size(Branch(Leaf(0), Leaf(0)))),
      (5, size(Branch(Branch(Leaf(0), Leaf(0)), Leaf(0))))
    ))
    test("max", Seq(
      (1, max(Leaf(1))),
      (3, max(Branch(Leaf(0), Leaf(3)))),
      (5, max(Branch(Branch(Leaf(0), Leaf(5)), Leaf(3))))
    ))
    test("depth", Seq(
      (1, depth(Leaf(1))),
      (2, depth(Branch(Leaf(0), Leaf(3)))),
      (3, depth(Branch(Branch(Leaf(0), Leaf(5)), Leaf(3))))
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