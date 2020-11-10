import cats.Monoid
import cats.instances.int._
import cats.instances.list._
import cats.syntax.semigroup._

object SuperAdder {
  implicit val intAdditonMonoid: Monoid[Int] =
    new Monoid[Int] {
      def empty: Int = 0 
      def combine(x: Int, y: Int): Int = x + y
    }

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderAdditionMonoid: Monoid[Order] = 
    new Monoid[Order] {
      def empty = Order(0.0, 0.0)
      def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity) 
    }
  
  def add[A: Monoid](items: List[A]): A = {
    items.foldLeft(Monoid[A].empty)(Monoid[A].combine)
  }


  def addCats[A: Monoid](items: List[A]): A = {
    items.reduce(_ |+| _)
  }

  def run = {
    val a = Order(1.0, 2.0)
    val b = Order(3.0, 5.0)

    println(add(List(a, b)))
    println(addCats(List(a, b)))
  }
}
