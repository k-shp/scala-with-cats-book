trait Semigroup[A]{
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid 

  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a && b
      def empty = true 
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a || b
      def empty = false
    }    

  implicit val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = (a && !b) || (!b && a)
      def empty = false 
    }

  implicit val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = (!a || b) && (a || !b) 
      def empty = true 
    }

  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a union b
      def empty = Set.empty[A]
    }
}

object RunMonoid{
  println(Monoid[Set[Int]].combine(Set(1,2), Set(3,4)))
  println(Monoid[Boolean](Monoid.booleanAndMonoid).combine(true, false))
}
