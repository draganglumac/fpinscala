package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Consulted answers but mostly to check whether there's a way to define 'take' using combinators
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // Peek at answers to double-check and had to fix a bug
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => empty
  }

  // Peek at answers to double-check and had to fix a bug
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)
  
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  // Incomplete solution, doesn't account for 'this' being shorter than 's'.
  def startsWithDG[B >: A](s: Stream[B]): Boolean =
    zipWith(s)((_, _)).foldRight(true)((as, b) => as._1 == as._2 && b)

  // After peeking at the solution - forgot about 'forAll' and 'exist'
  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(as => as._2.isDefined).forAll(as => as._1 == as._2)

  def tails: Stream[Stream[A]] =
    unfold(this) {
                   case Empty => None
                   case as@Cons(h, t) => Some(as, t())
                 }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def toList: List[A] =
    foldRight[List[A]](Nil)((h, acc) => h :: acc)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](as: Stream[B]): Stream[B] =
    foldRight(as)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
                   case Cons(h, t) => Some(f(h()), t())
                   case _ => None
                 }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
                        case (Cons(h, t), nn) if nn > 0 => Some(h(), (t(), nn - 1))
                        case _ => None
                      }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
                   case Cons(h, t) if f(h()) => Some(h(), t())
                   case _ => None
                 }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
                         case (Cons(a, t1), Cons(b, t2)) => Some(f(a(), b()), (t1(), t2()))
                         case _ => None
                       }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
                         case (Cons(a, t1), Cons(b, t2)) => Some((Some(a()), Some(b())), (t1(), t2()))
                         case (Cons(a, t), Empty) => Some((Some(a()), None), (t(), empty))
                         case (Empty, Cons(b, t)) => Some((None, Some(b())), (empty, t()))
                         case _ => None
                       }

  def zipAll2[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWith(s2)((a, b) => (Some(a), Some(b)))

  // Caching variant - lifted from the answers after a decent attempt at a non-caching version using tails and foldRs.
  // The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results,
  // which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold
  // than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((res, acc) => {
      lazy val lazyAcc = acc
      val b = f(res, lazyAcc._1)
      (b, cons(b, lazyAcc._2))
    })._2
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def fibs: Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = cons(n1, go(n2, n1 + n2))
    go(0, 1)
  }

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(s => Some((s, s)))

  val onesViaUnfold: Stream[Int] =
    constantViaUnfold(1)
}