package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Ex. 3.25

  def size_mine[A](t: Tree[A]): Int = {

    // TODO tailrecにしたい
    def loop(u: Tree[A], acc: Int): Int = u match {
      case Leaf(_) => acc+1
      case Branch(l, r) => loop(l, acc+1) + loop(r, 0)
    }

    loop(t, 0)

  }

  // official の方が簡潔だし、左右対称でキレイ
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }



}
