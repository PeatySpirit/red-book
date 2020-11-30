package org.example.redbook

import org.example.redbook.chapter4.errorhandling._

object Chapter4 extends App {

  val o: Option[Int] = Some(1)

  o.map(_ * 2)

}
