final case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String 
}

object PrintableInstances {
  implicit val printableString: Printable[String]  = new Printable[String] {
    def format(value: String): String = value
  }
   
  implicit val printableInt: Printable[Int] = new Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit val printableCat: Printable[Cat] = new Printable[Cat] {
    def format(value: Cat): String = 
      s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String  = {
    p.format(value) 
  }

  def print[A](value: A)(implicit p: Printable[A]): Unit = {
    println(format(value))
  }
}


object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printableInstance: Printable[A]): String = {
      printableInstance.format(value) 
    }

    def print(implicit printableInstance: Printable[A]): Unit = {
      println(format)
    }
  }
}

object Run {
  val matroskin = Cat("Matroskin", 4, "grey with stripes")

  def usingInterfaceObjectsApproach = {
    import PrintableInstances._
    Printable.print(matroskin)
  }

  def usingInterfaceSyntaxApproach = {
    import PrintableInstances._
    import PrintableSyntax._
    matroskin.print
  }

  def usingCats = {
    import cats.Show
    import cats.implicits._
    implicit val catShow: Show[Cat] = new Show[Cat] {
      def show(cat: Cat): String = 
        s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
    }
    matroskin.show
  }
}


