package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`,
// since we are writing our own in this chapter
import scala.{Option => _, Some => _, Either => _, _}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

// Ex. 4.8
object Person{

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  //test
  def main(args: Array[String]): Unit = {

    val errBoth = mkPerson("",-1)
    println(errBoth)

    val errAge = mkPerson("Valid Name",-1)
    println(errAge)


  }

}
