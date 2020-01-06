package org.example.redbook.chapter4.errorhandling

sealed trait Option[+Q] {
  //EXERCISE 4.1
  def map[B](f: Q => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None    => None
  }

  def flatMap[B](f: Q => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None    => None
  }

  def getOrElse[B >: Q](default: => B): B = this match {
    case Some(v) => v
    case None    => default
  }

  def orElse[B >: Q](ob: => Option[B]): Option[B] = this match {
    case Some(v) => Some(v)
    case None    => ob
  }

  def filter(f: Q => Boolean): Option[Q] = this match {
    case Some(v) if f(v) => this
    case _               => None
  }

  //EXERCISE 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(aa), Some(bb)) => Some(f(aa, bb))
      case _                    => None
    }

  def map2withFlatMap[A, B, C](a: Option[A],
                               b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  //EXCERCISE 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil    => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  /*
  It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
  Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
  unfortunate consequence of Scala using subtyping to encode algebraic data types.
   */
  def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}
