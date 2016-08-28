package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(f: A => Boolean): Boolean =
    foldRight(false)((a, b) => f(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `f(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => Nil
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }

  def takeWhileFold(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (f(a)) cons(a, b)
      else empty
    )

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Option(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")


  def mapUn[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def takeUn(n: Int): Stream[A] =
    unfold((this, n)){
      case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i - 1)))
      case _ => None
    }

  def takeWhileUn(f: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWithUn[B,C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some((
        f(h1(), h2()),
         (t1(), t2())
      ))
      case _ => None
    }

  // def zipAll[B](that: Stream[B]): Stream[(Option[A],Option[B])] =
  //   unfold((this, that)){
  //   }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def doFib(prev: Int, cur: Int): Stream[Int] =
      cons(cur, doFib(cur, prev + cur))
    doFib(0, 1)
  }

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): Stream[A] =
    f(state) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def onesUn: Stream[Int] = unfold(1)(s => Some(1, 1))
  def constantUn[A](a: A): Stream[A] = unfold(a)(s => Some(a, a))
  def fromUn(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))
  def fibUn: Stream[Int] = unfold((0, 1)){
    case (cur, next) => Some(cur, (next, cur + next))
  }
}
