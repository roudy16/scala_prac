package fpinscala.datastructures

sealed trait PracList[+A] {
  override def toString(): String = {
    this match {
      case Nil => "EmptyList"
      case Cons(x, xs) => PracList.foldLeft(xs, x.toString())((b, a) => b + ", " + a.toString())
    }
  }
}

case object Nil extends PracList[Nothing]
case class Cons[+A](head: A, tail: PracList[A]) extends PracList[A]

object PracList {
  def apply[A](as: A*): PracList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def length[A](ls: PracList[A]): Int =
    foldLeft(ls, 0)((b, _) => b + 1)

  def sum(ints: PracList[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product(nums: PracList[Double]): Double =
    foldLeft(nums, 1.0)(_ * _)

  @annotation.tailrec
  def dropWhile[A](as: PracList[A])(f: A => Boolean): PracList[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }

  def foldRight[A, B](as: PracList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldRightAlt[A, B](as: PracList[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(_, _) => foldLeft(reverse(as), z)((c: B, d: A) => f(d, c))
    }
  }

  @annotation.tailrec
  def foldLeft[A, B](as: PracList[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](as: PracList[A]): PracList[A] =
    foldLeft(as, PracList[A]())((ls, a) => Cons(a, ls))

  def append[A](lhs: PracList[A], rhs: PracList[A]): PracList[A] = {
    if (lhs == Nil) rhs
    else if (rhs == Nil) lhs
    else foldLeft(reverse(lhs), rhs)((r: PracList[A], a: A) => Cons(a, r))
  }

  def concat[A](as: PracList[PracList[A]]): PracList[A] =
    foldRightAlt(as, PracList[A]())(append)

  def map[A, B](as: PracList[A])(f: A => B): PracList[B] =
    foldRightAlt(as, PracList[B]())((a, acc) => Cons(f(a), acc))

  def flatMap[A, B](as: PracList[A])(f: A => PracList[B]): PracList[B] =
    concat(map(as)(f))

  def filter[A](as: PracList[A])(f: A => Boolean): PracList[A] =
    flatMap(as)((a: A) => if (f(a)) PracList(a) else Nil)

  def zipAdd(as: PracList[Int], bs: PracList[Int]): PracList[Int] =
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipAdd(xs, ys))
    }

  def zipWith[A, B, C](as: PracList[A], bs: PracList[B])(f: (A, B) => C): PracList[C] =
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  @annotation.tailrec
  def startsWith[A](as: PracList[A], sub: PracList[A]): Boolean =
    (as, sub) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) startsWith(xs, ys) else false
      case _ => false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: PracList[A], sub: PracList[A]): Boolean =
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) startsWith(xs, ys) else hasSubsequence(xs, sub)
    }

  def add1(as: PracList[Int]): PracList[Int] =
    foldRight(as, PracList[Int]())((a, acc) => Cons(a + 1, acc))
}