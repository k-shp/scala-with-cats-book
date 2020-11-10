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
}

