package org.example.redbook.chapter3.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
final case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  @tailrec
  private def sumTailRec(list: List[Int], accumulator: Int): Int = {
    list match {
      case Nil         => accumulator
      case Cons(x, xs) => sumTailRec(xs, accumulator + x)
    }
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Cons(_, tail) => tail
    case Nil           => sys.error("tail of empty list")
  }

  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Nil           => List(newHead)
    case Cons(_, tail) => Cons(newHead, tail)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case 1 => tail(l)
    case x => drop(tail(l), x - 1)
  }

  @tailrec
//  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
//    case Nil         => Nil
//    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else l
//  }
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def init2[A](l: List[A]): List[A] =
    reverse(tail(reverse(l)))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight2(xs, z)(f))
    }

  @tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B =
    list match {
      case Nil         => z
      case Cons(a, as) => foldLeft(as, f(z, a))(f)
    }

//  //Can you write foldLeft in terms of foldRight?
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))
//  //Can you write foldRight in terms of foldLeft?
//  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = ???

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => 1 + y)
//  foldRight(as, 0)((_,acc) => acc + 1)

  //Write sum, product, and a function to compute the length of a list using foldLeft.
  def sum3(xs: List[Int]): Int =
    foldLeft(xs, 0)(_ + _)

  def product3(xs: List[Double]): Double =
    foldLeft(xs, 1.0)(_ * _)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  def reverse[A](list: List[A]): List[A] =
    foldLeft(list, Nil: List[A])((x, y) => Cons(y, x))

  def append[A](list1: List[A], list2: List[A]): List[A] =
    foldRight(list1, list2)(Cons(_, _))

  /*EXERCISE 3.15
  Hard: Write a function that concatenates a list of lists into a single list. Its runtime
    should be linear in the total length of all lists. Try to use functions we have already
  defined.*/
  def concat[A](lists: List[A]*): List[A] =
    if (lists.isEmpty) Nil
    else append(lists.head, concat(lists.tail: _*))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def add1(list: List[Int]): List[Int] =
    foldRight(list, Nil: List[Int])((x, y) => Cons(x + 1, y))

  def toString(ld: List[Double]): List[String] =
    foldRight(ld, Nil: List[String])((x, y) => Cons(x.toString, y))

//  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
//    case Nil         => Nil
//    case Cons(x, xs) => Cons(f(x), map(xs)(f))
//  }
//  better tail rec implementation of map
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))

  //3.20
  //For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
  //List(1,1,2,2,3,3).
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((x, y) => if (f(x)) Cons(x, y) else y)

  //3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  //EXERCISE 3.22
  def zipNumbers(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
      case (Cons(leftH, leftT), Cons(rightH, rightT)) =>
        Cons(leftH + rightH, zipNumbers(leftT, rightT))
      case (Cons(leftH, leftT), Nil)   => Cons(leftH, zipNumbers(leftT, Nil))
      case (Nil, Cons(rightH, rightT)) => Cons(rightH, zipNumbers(rightT, Nil))
      case (Nil, Nil)                  => Nil
    }

  //EXERCISE 3.23
  /**
    *
    * @param left List[A] left list to merge
    * @param right List[A] right list to merge
    * @param z how to treat a Nil when left and right don't have the same length
    * @param f function to zip elements from left and right Lists
    * @return new List that is a combination of left and right using function f
    */
  def zipWith[A, B](left: List[A], right: List[A], z: A)(
    f: (A, A) => B
  ): List[B] =
    (left, right) match {
      case (Cons(leftH, leftT), Cons(rightH, rightT)) =>
        Cons(f(leftH, rightH), zipWith(leftT, rightT, z)(f))
      case (Cons(leftH, leftT), Nil) =>
        Cons(f(leftH, z), zipWith(leftT, Nil, z)(f))
      case (Nil, Cons(rightH, rightT)) =>
        Cons(f(rightH, z), zipWith(rightT, Nil, z)(f))
      case (Nil, Nil) => Nil
    }

  /** EXERCISE 3.24
    * Hard: As an example, implement hasSubsequence for checking whether a List contains
    * another List as a subsequence. For instance, List(1,2,3,4) would have
    * List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
    * some difficulty finding a concise purely functional implementation that is also efficient.
    * That’s okay. Implement the function however comes most naturally. We’ll
    * return to this implementation in chapter 5 and hopefully improve on it. Note: Any
    * two values x and y can be compared for equality in Scala using the expression x == y.
    */
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil)          => false
      case (Nil, _)          => false
      case (Cons(_, Nil), _) => sup == sub
      case (Cons(_, _), Cons(_, _)) =>
        if (takeN(sup, length(sub)) == sub) true
        else hasSubsequence(tail(sup), sub)
    }
  }

  /**
    * Returns first N elements of list
    */
  def takeN[A](list: List[A], n: Int): List[A] = n match {
    case 0 => Nil //take 0 first elements from a list
    case 1 => head(list)
    case x => concat(head(list), takeN(tail(list), x - 1))
  }

  def head[A](list: List[A]): List[A] = list match {
    case Nil        => Nil
    case Cons(x, _) => List(x)
  }
  // EXERCISE 3.24

  //if (n == 0) list else concat(takeN(list, n - 1))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
