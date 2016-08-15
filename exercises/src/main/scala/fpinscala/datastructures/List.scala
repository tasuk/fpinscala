package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](list: List[A], acc: B)(f: (A, B) => B): B = // Utility functions
    list match {
      case Nil => acc
      case Cons(head, tail) => f(head, foldRight(tail, acc)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def head[A](l: List[A]): A =
    l match {
      case Cons(h, _) => h
      case _ => sys.error("head of empty list")
    }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(_, t) => t
      case _ => sys.error("tail of empty list")
    }

  def setHead[A](l: List[A], h: A): List[A] = sys.error("todo")

  def drop[A](l: List[A], n: Int): List[A] = sys.error("todo")

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = sys.error("todo")

  def init[A](l: List[A]): List[A] = ???

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B)(f: (B, A) => B): B =
    l match {
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x))(f)
    }

  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]) =
    foldLeft(l, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((acc,h) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc: List[A], h: A) => Cons(h, acc))

  def foldRightTail[A,B](l: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), acc)((b,a) => f(a,b))

  def append3[A](a1: List[A], a2: List[A]): List[A] = {
    def run(a1: List[A], a2: List[A]): List[A] = {
      a1 match {
        case Nil => a2
        case Cons(x, xs) => run(xs, Cons(x, a2))
      }
    }
    run(reverse(a1), a2)
  }

  def append4[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((a,b) => Cons(b,a))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
    foldRight(l, r)(Cons(_,_))

  def flatten[A](l: List[List[A]]): List[A] = {
    def run(from: List[List[A]], acc: List[A]): List[A] =
      from match {
        case Cons(h, t) => run(t, append4(h, acc))
        case _ => acc
      }
    run(reverse(l), Nil)
  }

  def flatten2[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append4)

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((item, acc) => Cons(item + 1, acc))

  def ldts(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((item, acc) => Cons(item.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((item, acc) => Cons(f(item), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
      case Cons(h, t) => filter(t)(f)
      case Nil => Nil
    }

  def filter3[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((item, acc) =>
      if (f(item)) Cons(item, acc)
      else acc
    )

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  def filter4[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(item => if (f(item)) List(item) else Nil)

  def addLists(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
      case _ => Nil
    }

  def zipWith[A,B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] =
    (l1, l2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }

  def hasSubsequenceWrong[A](sup: List[A], sub: List[A]): Boolean = {
    val matches: Boolean = sub match {
      case Nil => true
      case Cons(h, t) if length3(sup) > 0 && h == head(sup) => hasSubsequenceWrong(tail(sup), t)
      case _ => false
    }

    if (matches) true
    else if (length3(sup) == 0) false
    else hasSubsequenceWrong(tail(sup), sub)
  }
}
