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

import PrintableInstances._

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
  // Interface Approach 1: Interface Objects
  //import PrintableInstances._
  //val matroskin = Cat("Matroskin", 4, "grey with stripes")
  //Printable.print(matroskin)
  

  // Interface Approach 2: Interface Syntax
  import PrintableInstances._
  import PrintableSyntax._
  val matroskin = Cat("Matroskin", 4, "grey with stripes")
  matroskin.print
}


