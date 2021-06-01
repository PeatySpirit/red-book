package chapter6.functional_state

import com.github.peatyspirit.redbook.chapter6.functional_state.RNG
import com.github.peatyspirit.redbook.chapter6.functional_state.RNG._
import org.scalatest._
import flatspec._
import matchers._


class RNGTest extends AnyFlatSpec with should.Matchers  {
  private val rngSeed = SimpleRNG(42).nextInt

  private val streamRNGnonNegative:LazyList[(Int, RNG)] =
    LazyList.iterate(rngSeed)(x => nonNegativeInt(x._2))
  println(streamRNGnonNegative.take(10).toList)

  private val limitedStream: Seq[(Int, RNG)] = streamRNGnonNegative.take(10000)

  all (limitedStream.map(_._1)) should be > 0
  limitedStream shouldEqual limitedStream.distinct
}
