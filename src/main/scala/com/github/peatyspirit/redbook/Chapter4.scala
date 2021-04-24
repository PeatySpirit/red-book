package com.github.peatyspirit.redbook

import com.github.peatyspirit.redbook.chapter4.errorhandling.{Option, Some}

object Chapter4 extends App {

  val o: Option[Int] = Some(1)

  o.map(_ * 2)

}
