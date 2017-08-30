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

  // 結局tailrecにできないなら、official の方が簡潔だし、左右対称でキレイ
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }


  // Ex. 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  // Ex. 3.27
  def depth_mine[A](t: Tree[A]): Int = {
    def loop(u: Tree[A], acc: Int): Int = u match {
      case Leaf(_) => acc+1 // officialの定義は「いきなりLeafのときは0」という数え方ぽいので、であれば+1は不要
      case Branch(l, r) => loop(l, acc+1) max loop(r, acc+1)
    }
    loop(t, 0)
  }
  // Official
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  // Ex. 3.28
  def map_mine[A, B](t: Tree[A])(f: Tree[A] => Tree[B]): Tree[B] = t match {
    case Leaf(_) => f(t)
    case Branch(l,r) => Branch(map_mine(l)(f), map_mine(r)(f))
  }

  // Officialの方が簡潔
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }


  // Ex. 3.29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B) : B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g( fold(l)(f)(g) ,  fold(r)(f)(g) )
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)( _ => 1 )( 1 + _ + _ )

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)( a => a )( _ max _ )

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)( _ => 0 )( (l, r) => 1 + ( l max r ) )

  def mapViaFold[X, Y](t: Tree[X])(h: X => Y): Tree[Y] =
    fold(t)( a => Leaf(h(a)): Tree[Y] )( (l, r) => Branch( l , r ) )


}
