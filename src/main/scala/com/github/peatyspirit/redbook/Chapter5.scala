package com.github.peatyspirit.redbook

import com.github.peatyspirit.redbook.chapter5.strictness_laziness.Stream

object Chapter5 extends App {

  println("Strictness and laziness")

  println("Streams are lazy - nothing should be seen here but due to implementation of constructor it gets strictly evaluated")
  val streamNotSoLazy = Stream({
    println(1)
  }, {
    println(2)
  }, {
    println(3)
  })

  println("Lazier stream using smart constructors or #:: can be used where only head get evaluated in before")
  val streamLazier = Stream.cons({
    println("a"); "a"
  }, Stream.cons({
    println("b"); "b"
  }, Stream.cons({
    println("c"); "c"
  }, Stream.empty)))

  val stream = Stream(1,2,3,4,5)

  println("EXERCISE 5.1 Stream to List")
  println(stream)
  val list = stream.toList
  println(list)

  println("EXERCISE 5.2 takeN dropN")
  println(stream.take(2).toList)
  println(stream.drop(2).toList)

  println("EXERCISE 5.3 takeWhile")
  println(stream.takeWhile(_ < 5).toList)

  println("EXERCISE 5.4 forAll should evaluate just first non-matching value")
  streamLazier.forALl(_ == "a")

  println("EXERCISE 5.7 map using implemented via foldRight")
  println(stream.map(_ * 2).toList)

  println("EXERCISE 5.7 should filter odd numbers")
  println(stream.filter(_ % 2 == 0).toList)

  println("Example of working with infinite streams")
  val ones: Stream[Int] = Stream.constant(1)
  println(ones.take(5).toList)
  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(ones.takeWhile(_ == 1))
  println(ones.forALl(_ != 1))
  // stack overflow: println(ones.forALl(_ == 1))

  println("EXERCISE 5.9 infinite stream of Integers")
  println(Stream.from(100).take(3).toList)

  println("EXERCISE 5.10 Fibonacci numbers")
  println(Stream.fibs().take(10).toList)

  println("EXERCISE 5.11 unfold countdown from 10 to 1")
  println(Stream.unfold(10)(count =>
    if (count > 0) Some((count, count - 1))
    else None).toList)

  println("EXERCISE 5.12 unfold implementations")
  println(Stream.fibs().take(10).toList)
  println(Stream.from2(5).take(6).toList)
  println(Stream.constant2("I am a constant in this infinite Stream").take(3).toList)
  println(Stream.ones2.take(3).toList)

  println("EXERCISE 5.13 unfold fibs")
  println(ones.mapUnfold(_ + 1).exists(_ % 2 == 0))
  println(ones.takeUnfold(2).toList)
  println(stream.takeWhileUnfold(_ < 5).toList)
  println(stream.zipWithUnfold(ones)(_ + _).toList)
  println(stream.zipAllUnfold(ones).take(10).toList)
}
