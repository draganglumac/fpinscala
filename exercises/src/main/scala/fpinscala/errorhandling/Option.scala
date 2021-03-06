package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(b) => b
  }

  // Stolen from the answers - makes perfect sense now that I know the answer!
  // I feel a bit annoyed with myself because I knew the solution had to combine
  // 'map' and 'getOrElse' in some way but I couldn't think of the right LEGO
  // because I was focused on how to extract 'A' from 'this' with 'getOrElse',
  // and did not consider Option[B] as the return type for the argument of 'map'.
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMapPattern[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def mapViaFlatMap[B](f: A => B): Option[B] =
    flatMap(a => Some(f(a)))

  // Stolen from the answers - I am struggling to wrap my brain round combining primitives...
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(a => Some(a)).getOrElse(ob)

  def orElsePattern[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => this
  }

  // Yay! Only had an inkling I needed 'flatMap' and I figured it out. Success!!!
  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) this else None)

  def filterPattern(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  
  def lift[B >: A, C](f: B => C): Option[B] => Option[C] =
    _ map f
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (a1 => b map (b1 => f(a1, b1)))

  // Giving up... My brain is struggling with this one. Checking the answers.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  // Half-stolen
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case ha :: ta => f(ha) flatMap (b => traverse(ta)(f) map (b :: _))
  }

  // Stolen
  def traverseViaFold[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  // Stolen
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}