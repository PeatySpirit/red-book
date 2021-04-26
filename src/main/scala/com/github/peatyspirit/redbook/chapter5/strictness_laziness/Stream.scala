package com.github.peatyspirit.redbook.chapter5.strictness_laziness

import com.github.peatyspirit.redbook.chapter5.strictness_laziness.Stream.empty

import Stream._
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  // Returns n first elements of a stream
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 =>cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // Returns all starting elements that match the predicate p
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Skips first n elements of a stream
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n-1)
    case Cons(_, t) if n == 1 => t()
    case _ if n < 1 => this
  }

  // Returns true if there exists at least one element fitting the predicate p
  def exists(p: A => Boolean): Boolean = this match {
    // Thanks to implementation of || it will not keep traversing after first occurrence is found
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // 5.4 Checks if all elements in the stream match a given predicate, terminates once it finds first non-matching value
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5 Implement takeWhile using foldRight
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else      empty)

  // 5.6 HARD Implement headOption using foldRight
  def headOption2: Option[A] = ???

  // 5.7 Implement map, filter, append and flatMap using foldRight, append should be non-strict in its argument
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  // EXERCISE 5.13 Use unfold to implement map, take, takeWhile, zipWith, zipAll
  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n >= 1 => Some(h(), (t(), n - 1))
      case _ => None
    }

  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWithUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAllUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  // EXERCISE 5.14 HARD Implement startsWith using previous functions
  // TODO investigate why this has some complaints from Idea about type shadowing
  def startsWith[A](s2: Stream[A]): Boolean =
    this.zipAllUnfold(s2).forAll {
      case (Some(x), Some(y)) => x == y
      case (None, Some(_)) => false
      case _ => true
    }

  // EXERCISE 5.15 Implement tails using unfold
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case x => Some((x, x.drop(1) ))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists(_ startsWith s)
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

  // EXERCISE 5.8 infinite Stream of given number
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // EXERCISE 5.9 infinite Stream of Integers incremented by 1
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // EXERCISE 5.10 infinite Stream of Fibonacci numbers
  def fibs(): Stream[Int] = {
    def internalLoop(prev: Int, current: Int): Stream[Int] = cons(current, internalLoop(current, prev + current))
    cons(0, internalLoop(0, 1))
  }

  // EXERCISE 5.11 generic Stream building function unfold
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // EXERCISE 5.11 implement fibs, from, constant and ones in terms of unfold
  def fibs2(): Stream[Int] =
    unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2)))
  def from2(start: Int): Stream[Int] = unfold(start)(n => Some(n, n + 1))
  def constant2[A](c: A): Stream[A] = unfold(c)(_ => Some(c, c))
  def ones2(): Stream[Int] = constant2(1)

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
