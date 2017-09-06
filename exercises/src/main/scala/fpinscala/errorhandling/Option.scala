package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`,
// since we are writing our own in this chapter
import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match{
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match{
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)


  def orElse_mine_usingPatternMatching[B>:A](ob: => Option[B]): Option[B] = this match{
    case None => ob
    case _ => this
  }
  // Official
  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  // None if not f
  def filter(f: A => Boolean): Option[A] =
    flatMap( a => (if (f(a)) Some(a) else None) )

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
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  // Ex. 4.2
  def variance_mine(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    m.flatMap( (d: Double) => mean( xs.map( (x: Double) => math.pow(x - d, 2))))
  }
  // 中間変数をなくし、不要な型アノテーションを消すと、OfficialAnswerになる
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


  // Ex. 4.3
  //def map2_mine[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  //  a.map(a => (b: Option[B] => f(a, b)) )(b) // compile error

  // Official answer.
  // curryingするという点は合ってた模様。
  //
  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  // Ex. 4.4
  def sequence_mine[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case h :: t => {
        (h, sequence(t)) match {
          case (Some(hh), Some(tt)) => Some(hh :: tt)
        }
    }
    case _ => None // case None :: _ or Nil or None
  }
  /*
  Here's an explicit recursive version:
  Optionの皮をむいてから処理したいなら
  map（与える関数は A => B つまり失敗想定ナシ だが、
      そこに入るaがNoneならNoneを返したいので、戻り値はOption）や
  flatMap（与える関数は A => Option[B] 。map結果を右側に置きたいならこれになる）
  を使えばよい
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  /*
  It can also be implemented using `foldRight` and `map2`.
  The type annotation on `foldRight` is needed here;
  otherwise Scala wrongly infers the result type of the fold
  as `Some[Nil.type]` and reports a type error (try it!).

  This is an unfortunate consequence of Scala using subtyping
  to encode algebraic data types.
  */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    // a.foldRight(Some(Nil))((x,y) => map2(x,y)(_ :: _)) // > try it!
    a.foldRight(Some(Nil):Option[List[A]])((x,y) => map2(x,y)(_ :: _))


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
