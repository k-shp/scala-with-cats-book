import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
import cats.data.Writer
import cats.instances.vector._ 
import cats.syntax.applicative._
import cats.syntax.writer._ 

object WriterMonadExercise {
  type Logged[A] = Writer[Vector[String], A]
  
  def slowly[A](body: => A) = try body finally Thread.sleep(250)

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) 1.pure[Logged]
             else slowly(factorial(n-1).map( _ * n))
      _ <- Vector(s"fact $n is $ans").tell 
    } yield ans
  }
  

  
  def main(args: Array[String]): Unit = {
    val res = Await.result(Future.sequence(Vector(
      Future(factorial(5))
    )), 5.seconds)
    println(res)
  }
}
