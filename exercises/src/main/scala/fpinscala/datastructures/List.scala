package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sumR(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def productR(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Cannot call tail on an emtpy list.")
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Cannot setHead on an empty list.")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => sys.error("Too many elements to drop $n")
      case Cons(h, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of an empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumL(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productL(ns: List[Int]) =
    foldLeft(ns, 1.0)(_ * _)

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, h) => Cons(h, acc))

  def foldLeftR[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z) { (a, b) => println(s"f($b, $a)"); f(b, a) }

  // [DG] Lifted from answers - see comment beginning on line 177 in answers project
  def foldLeftByRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightL[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z) { (b, a) => println(s"f($a, $b)"); f(a, b) }

  // [DG] Lifted from answers - see comment beginning on line 177 in answers project
  def foldRightByLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def appendF[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def concatenate[A](ls: List[List[A]]): List[A] =
    foldLeft(ls, Nil: List[A])(append)

  def add1(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  def doublesToStrings(dubs: List[Double]): List[String] = dubs match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doublesToStrings(t))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) Cons(h, filter(t)(f))
      else filter(t)(f)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def mapViaFold[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a,b) => Cons(f(a), b))

  def filterViaFold[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((x,xs) => if (f(x)) Cons(x, xs) else xs)

  def flatMapViaFold[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((a, bs) => append(f(a), bs))

  def flatMapViaMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concatenate(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(ah, at) => bs match {
      case Nil => Nil
      case Cons(bh, bt) => Cons(ah + bh, addPairwise(at, bt))
    }
  }

  def addPairwiseNicer(as: List[Int], bs: List[Int]): List[Int] = (as,bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, at), Cons(b, bt)) => Cons(a + b, addPairwiseNicer(at, bt))
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // Pretty much stolen from answers
  @annotation.tailrec
  def startsWith[A](as: List[A], prefix: List[A]): Boolean = (as, prefix) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
    case _ => false
  }

  // Pretty much stolen from answers
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}
