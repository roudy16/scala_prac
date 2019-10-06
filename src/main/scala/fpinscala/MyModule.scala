package fpinscala

import fpinscala.datastructures.{Branch, Leaf, PracList}

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def fact(n: Int): Int = {
    @annotation.tailrec
    def go(x: Int, acc: Int): Int = {
      if (x <= 0) acc
      else go(x - 1, x * acc)
    }
    go(n, 1)
  }

  def fib(x: Int): Int = {
    @annotation.tailrec
    def tr(a: Int, b: Int, n: Int, sent: Int): Int = {
      var temp = a + b
      if (n == sent) temp
      else tr(b, temp, n + 1, sent)
    }
    if (x == 1) 0
    else if (x == 2) 1
    else tr(0, 1, 3, x)
  }

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFact(n: Int): String = {
    val msg = "The factorial of %d is %d."
    msg.format(n, fact(n))
  }

  private def formatFib(n: Int): String = {
    val msg = "The %d number in the Fibinacci sequence is %d"
    msg.format(n, fib(n))
  }

  def formatResult(n: Int, name: String, f: Int => Int): String = {
    val msg = "The result of %s with input %d is %d"
    msg.format(name, n, f(n))
  }

  def genericComp[A](e: A): A => Boolean = {
    new Function1[A, Boolean] {
      def apply(q: A): Boolean = { q == e }
    }
  }

  def matchFunc[A]: (A, A) => Boolean = {
    new Function2[A, A, Boolean] {
      def apply(a: A, b: A): Boolean = { a == b }
    }
  }

  def findElement[A](qs: Array[A], comp: A => Boolean): Int = {
    @annotation.tailrec
    def loop(i: Int): Int = {
      if (i >= qs.length) -1
      else if (comp(qs(i))) i
      else loop(i + 1)
    }
    loop(0)
  }

  def findElement[A](qs: Array[A], comp: (A, A) => Boolean, q: A): Int = {
    @annotation.tailrec
    def loop(i: Int): Int = {
      if (i >= qs.length) -1
      else if (comp(q, qs(i))) i
      else loop(i + 1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    // Making a List ...must ...not ...sing Christmas song
    val is0 = PracList(5, 3, 2, 1, 4)
    val is1 = PracList(8, 9, 5, 6, 6)
    val is2 = PracList(11, 41, 55, 54, 33, 21)
    val ds0 = PracList(5.0, 3.0, 2.0, 1.0, 4.0)
    val ds1 = PracList(8.0, 9.0, 5.0, 6.0, 6.0)
    val ds2 = PracList(11.0, 41.0, 55.0, 54.0, 33.0, 21.0)
    val isis = PracList(is0, is1, is2)
    val isis1 = PracList.concat(isis)
    val is01 = PracList.append(is0, is1)

    println(is01)

    // Setting up a Tree
    val tis0 = Leaf(0)
    val tis1 = Leaf(11)
    val tis2 = Leaf(2)
    val tis3 = Leaf(3)
    val b0 = Branch(tis0, tis1)
    val b1 = Branch(tis2, tis3)
    val root = Branch(b0, b1)

    val optInt = Some(3)

    println(optInt.filter((x: Int) => (x % 2) == 0).getOrElse(88))
  }

}
