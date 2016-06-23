package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => max(left).max(max(right))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(value) => 0
    case Branch(left, right) => (1 + depth(left)) max (1 + depth(right))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeF[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maxF(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthF[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1, d2) => (1 + d1) max (1 + d2))

  def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))
}