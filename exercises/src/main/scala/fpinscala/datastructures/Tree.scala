package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](tree: Tree[A])(lf: A => B)(bf: (B, B) => B): B =
    tree match {
      case Leaf(value) => lf(value)
      case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
    }

  def size2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(1 + _ + _)

  def maximum2(tree: Tree[Int]): Int =
    fold(tree)(v => v)(_ max _)

  def depth2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((l, r) => 1 + (l max r))

  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}
