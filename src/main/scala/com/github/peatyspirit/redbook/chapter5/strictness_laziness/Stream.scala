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

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
