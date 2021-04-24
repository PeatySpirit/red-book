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
}
