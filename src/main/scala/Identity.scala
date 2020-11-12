import cats.Id

object IdOps {
  def pure[A](a: A): Id[A] = a
  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)  
  def map[A,B](value: Id[A])(func: A => B): Id[B] = func(value)
  def run: Unit = {
    println(pure(1))
    println(flatMap(1)(_*2))
    println(map(1)(_*3))
  }
}
