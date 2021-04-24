package com.github.peatyspirit.redbook

object Chapter1 extends App {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(acc: Int, n: Int): Int = {
      if (n == 0) acc
      else loop(acc + n, n - 1)
    }

    loop(0, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(ar: Array[A], n: Int): Boolean = {
      if (n + 2 < ar.length && ordered(ar(n), ar(n + 1))) loop(ar, n + 1)
      else if (ordered(ar(n), ar(n + 1))) true
      else false
    }
    //loop the Array, start from the first element
    loop(as, 0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  //f compose g
  //g andThen f

  println(s"fibonacci(10) = ${fib(10)}")

  println(
    isSorted(
      Array(1, 2, 3, 2),
      (a: Int, b: Int) =>
        if (a <= b) true
        else false
    )
  )

  println(
    isSorted(Array(1, 2, 3, 4, 10, 15, 55, 55, 56), (a: Int, b: Int) => {
      if (a <= b) true
      else false
    })
  )
}
