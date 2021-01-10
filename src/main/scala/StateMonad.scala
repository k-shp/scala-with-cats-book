mport cats.data.State
import cats.syntax.applicative._ 

object StateMonadExercise {
  type CalcState[A] = State[List[Int], A]
  
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case n => operand(n.toInt)
  }
  
  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]) { (a, b) => 
      a.flatMap(_ => evalOne(b))
    }
  }
  
  def evalInput(input: String): Int = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }
  
  def operand(n: Int): CalcState[Int] = 
    State[List[Int], Int] {stack => 
      (n :: stack, n)
    }
  
  def operator(func: (Int, Int) => Int): CalcState[Int] = 
    State[List[Int], Int] {
      case b :: a :: tail => 
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ => sys.error("need two operands to pass to operator!")
    }
  
  
  def main(args: Array[String]): Unit = {
    val res = evalOne("4").runA(List()).value    
    val res2 = evalAll(List("2", "5", "+", "2", "*", "4",
"-")).runA(Nil).value
    val res3 = evalInput("2 3 + 3 *")
    println(res3)    
    
  }
}
