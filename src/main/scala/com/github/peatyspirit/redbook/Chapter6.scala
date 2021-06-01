package com.github.peatyspirit.redbook

import com.github.peatyspirit.redbook.chapter6.functional_state.RNG._

object Chapter6 extends App {

  println("Purely functional state")

  val rng = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt
  println(s"randomNumber: $n1 newRNG(seed): $rng2")

  val (n2, rng3) = rng2.nextInt
  println(s"randomNumber: $n2 newRNG(seed): $rng3")
}
