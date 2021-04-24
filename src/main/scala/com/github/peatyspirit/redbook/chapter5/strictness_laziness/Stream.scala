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

  // Returns if there exists at least one element fitting the predicate p
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
  def forALl(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => !p(h()) || t().forALl(p)
    case _ => true
  }

  // 5.5 Implement takeWhile using foldRight
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else      empty)

  // 5.6 HARD Implement headOption using foldRight
  def headOption2: Option[A] = ???

  // 5.7 Implement map, filter, append and flatMap using foldRight, append should be non-strict in its argument
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t)

  def append: Stream[A] = ??? // TODO not sure how to do that yet - having troubles even with signature...

  def flatMap[B](f: A => Stream[B]): Stream[B] = ???
    // cannot implement without append I guess
//    foldRight(empty[B])((h,t) => f(h) append t)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
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
