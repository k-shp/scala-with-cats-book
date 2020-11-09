import cats.Eq
import cats.Show
import cats.implicits._

object Equality {

  final case class Cat(name: String, age: Int, color: String)
  implicit val catEq: Eq[Cat] ={
    Eq.instance[Cat] { (catA, catB) => 
      catA.name === catB.name &&
      catA.age === catB.age &&
      catA.color === catB.color
    }
  }

  implicit val catShow: Show[Cat] = Show.fromToString[Cat]
  def run = {
    val cat1 = Cat("Leopold", 5, "orange")
    val cat2 = Cat("Gav", 1, "brown")
    val optionCat1 = Option(cat1)
    val optionCatNone = Option.empty[Cat]
    println(s"${cat1.show} === ${cat1.show}: ${cat1 === cat1}")
    println(s"${cat1.show} === ${cat2.show}: ${cat1 === cat2}")
    
    println(s"${optionCat1.show} === ${optionCatNone.show}: ${optionCat1 === optionCatNone}")
  }
}

