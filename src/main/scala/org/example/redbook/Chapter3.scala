package org.example.redbook

import org.example.redbook.chapter3.datastructures._
import org.example.redbook.chapter3.datastructures.List._

object Chapter3 {
  def main(args: Array[String]): Unit = {
    println(s"CHAPTER 3")
//    val x: Unit = List(1, 2, 3, 4, 5) match {
//      case Cons(x, Cons(2, Cons(4, _)))          => println(s"1: $x")
//      case Nil                                   => println(42)
//      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => println(s"$x + $y")
//      case Cons(h, t)                            => println(h + sum(t))
//      case _                                     => println(101)
//    }

    val li = List(1, 2, 3)
    val li2 = foldRight(li, Nil: List[Int])(Cons(_, _))
    println(li2 == li)
    println(length(li))
    println(length3(li))

    val xli = List(1, 2, 3)
    val xli2 = foldRight(xli, Nil: List[Int])(Cons(_, _))
    println(xli2 == xli)
    println(sum3(xli))
    println(s"reverse: ${reverse(xli)}")

    println(s"init List(1,2,3): ${init(li)}")
    println(s"init List(1,2,3): ${init2(li)}")

    val list5 = List(4, 5)
    println(s"append: ${append(li, list5)}")
    println(s"concat: ${concat(li, list5, List(6))}")
    println(s"map +1: ${map(li)(_ + 1)}")
    println(s"add1: ${add1(li)}")
    //EXERCISE 3.19
    println(s"filter odd numbers: ${filter(List(1, 2, 3, 4, 5))(_ % 2 == 0)}")
    println(map(List(1, 2, 3))(i => List(i, i)))
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    println(
      s"filter odd numbers with flapMap: ${filterWithFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0)}"
    )

    println(zipNumbers(li, List(1)))
    println(zipWith(li, List(1), 0)(_ + _))

    println("hasSubsequence(List(1,2,3), List(2,3)):")
    println(hasSubsequence(List(1, 2, 3), List(2, 3)))

  }
}
