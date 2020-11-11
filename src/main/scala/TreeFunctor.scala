import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Functors {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](value: Tree[A])(func: A => B): Tree[B] = {
      value match {
        case Leaf(v) => Leaf(func(v))
        case Branch(l, r) => Branch(map(l)(func), map(r)(func))
      }
    }
  }
}

object RunFunctors {
  def run = {
    val t: Tree[Int] = Branch(Leaf(1), Leaf(2))
    Functor[Tree].map(t)(_ * 2)
  }
}
