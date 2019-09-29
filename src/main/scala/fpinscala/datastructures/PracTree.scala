package fpinscala.datastructures

sealed trait PracTree[+A]
case class Leaf[A](value: A) extends PracTree[A]
case class Branch[A](left: PracTree[A], right: PracTree[A]) extends PracTree[A]

object PracTree {
  def size[A](t: PracTree[A]): Int =
    t match {
      case Branch(l, r) => size(l) + size(r) + 1
      case Leaf(_) => 1
    }

  def maximum(t: PracTree[Int]): Int =
    t match {
      case Branch(l, r) => maximum(l) max maximum(r)
      case Leaf(x) => x
    }

  def depth[A](t: PracTree[A]): Int =
    t match {
      case Branch(l, r) => (depth(l) max depth(r)) + 1
      case Leaf(_) => 0
    }

  def map[A, B](t: PracTree[A])(f: A => B): PracTree[B] =
    t match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(x) => Leaf(f(x))
    }

  /* TODO Incomplete, how to implement? Look up online
  def fold[A,B](t: PracTree[A])(f: PracTree[A]=>B): B =
    t match {
      case Branch(l, r) => fold(f(t))(f)
    
    }
  */
}