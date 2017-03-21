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

  def main(args: Array[String]): Unit = {
    test("size", Seq(
      (1, size(Leaf(0))),
      (3, size(Branch(Leaf(0), Leaf(0)))),
      (5, size(Branch(Branch(Leaf(0), Leaf(0)), Leaf(0))))
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