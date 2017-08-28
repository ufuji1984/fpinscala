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

  // @annotation.tailrec をつけると怒られるので確かにtailrecではない（Ex.3.10）
  // たしかにfoldRightした後でfに喰わせている。
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2

  /*
  <Official answer>

  Although we could return `Nil` when the input list is empty,
  we choose to throw an exception instead.
  This is a somewhat subjective choice.
  In our experience, taking the tail of an empty list is often a bug,
  and silently returning a value just means this bug will be discovered later,
  further from the place where it was introduced.

  It's generally good practice when pattern matching to use `_` for any
  variables you don't intend to use on the right hand side of a pattern.
  This makes it clear the value isn't relevant.
  */
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

  def tail_myAnswer[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil // or throw an exception?
      case Cons(x, xs) => xs
    }


  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead of empty list")
      case Cons(_, t) => Cons(h, t)
    }
  // 模範解答と一致した。

  // Exercise 3.4
  def drop_myAnswer[A](l: List[A], n: Int): List[A] =
    {
      @annotation.tailrec
      def go(lNow: List[A], doneCnt: Int): List[A] = {
        if (doneCnt >= n) lNow
        else go(tail(lNow), doneCnt+1)
      }

      go(l, 0)
    }
  // 模範解答は tail も helper method も使わずに自身を再帰で呼んでいる。
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 0)  sys.error("n < 0 for drop") // 模範解答ではlを返してる
    else if (n == 0) l
    else l match {
      case Nil => Nil // tailの説明を見ると初回なら例外のがよさげだが、再帰で呼ばれた場合はNilを返すのが正しい
      case Cons(_, t) => drop(t, n-1)
    }


  // Exercise 3.5
  /*
  Somewhat overkill, but to illustrate the feature we're using a _pattern guard_,
  to only match a `Cons` whose head satisfies our predicate, `f`.
  The syntax is to add `if <cond>` after the pattern, before the `=>`,
  where `<cond>` can use any of the variables introduced by the pattern.
  */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l // _ は任意の式とマッチする特殊なパターン
    }

  def dropWhile_mine[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => {
        if (f(h)) dropWhile(t, f)
        else l
      }
    }

  // Exercise 3.6
  def init_mine[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init on Nil")
      case Cons(h, t) => {
        t match {
          case Nil => Nil //Cons(h, Nil)
          case _ => Cons(h, init(t))
        }
      }
    }

  // Official1 ↑よりスッキリ（↑は無駄にmatchをネストしている）
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  // Exercise 3.7
  // この形なら短絡可能？
  def foldRight_withStopper[A,B](as: List[A], z: B, stopByZ: Boolean)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) if (x==z) => z // xとzは型が違うけどコンパイルはできるな。。。
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 3.9
  def length[A](l: List[A]): Int =
    //これでは常に1になってしまう foldRight(l, 0)((x,y) => 1)
    foldRight(l, 0)((_,acc) => acc + 1) // official answer


  // Exercise 3.10

  // 参考 def foldRight
  //          [A,B](l: List[A], z: B)(f: (A, B) => B): B
  //as match {
  //  case Nil => z
  //  case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //}

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      //mine case
      //   Cons(h,t) => f( foldLeft(t, z)(f) , h ))
      case Cons(h,t) => foldLeft(t, f(z,h))(f) //official
    }

  // Exercise 3.11
  def sum_byFoldLeft(ns: List[Int]) =
    //foldRight(ns, 0)((x,y) => x + y)
    foldLeft(ns, 0)(_ + _)

  def product_byFoldLeft(ns: List[Double]) =
    //foldRight(ns, 1.0)(_ * _)
    foldLeft(ns, 1.0)(_ * _)

  def length_byFoldLeft[A](l: List[A]): Int =
    //foldRight(l, 0)((_,acc) => acc + 1)
    foldLeft(l, 0)((acc,_) => acc + 1)

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
