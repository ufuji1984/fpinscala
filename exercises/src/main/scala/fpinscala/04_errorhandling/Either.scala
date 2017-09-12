package fpinscala.errorhandling

// hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, Left => _, Right => _, _}

sealed trait Either[+E,+A] {

  // Ex. 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e) // これは再度作ることにはならないのか？宣言がobjectだからSingleton的な動き？
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}


case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  // Ex. 4.7
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil) //mine Right(List())
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

/*
  def traverse_viaFoldRight[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Nil: B)( (a,b) => for {aa <- f(a); bb <- b} yield (aa :: bb) )
*/
  // Official
  def traverse_viaFoldRight[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))


  def sequence_viaMatching[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => (h map2 (sequence(t)))(_ :: _)
    }

  def sequence_viaFoldRight[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    // Type mismatches.foldRight(Right(Nil))((a, b) => a.map2(b)(_ :: _) )
    es.foldRight[Either[E,List[A]]](Right(Nil))((a, b) => a.map2(b)(_ :: _) )

  // Official
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)




  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
