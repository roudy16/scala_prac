package fpinscala.Option

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(x) => x
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(x) if f(x) => this
      case _ => None
    }
}

object Option {
  def variance(xs: Seq[Double]): Option[Double] = {
    xs.length match {
      case 0 => None
      case len =>
        val mean = xs.sum / len.toDouble
        val result = xs.map(x => math.pow(x - mean, 2))
          .sum / len.toDouble
        Some(result)
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Seq[Double](4,4,5,4,5,5,4,6,4,4,5,3,2,3,4,4,4)
    val nums0 = Seq[Double]()
    val v = variance(nums)
    val z = variance(nums0)
    println(v)
    println(z)
  }
}
