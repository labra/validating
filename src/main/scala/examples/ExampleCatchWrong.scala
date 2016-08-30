package examples
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import cats.implicits._

/*
object ExampleCatchWrong {
  
  type E = String
  type Comput = Fx.fx2[Validate[E, ?], Writer[E,?]]
  type Check[A] = Eff[Comput,A]
  
  def runCheck[A](c: Check[A]) = c.runNel.runWriter.run
  
  val comp1: Check[Int] = for {
      _ <- wrong[Comput,E]("1")
      _ <- wrong[Comput,E]("2")
  } yield 0
    
  val handle: E => Check[Int] = { case e => tell[Comput,E](e).as(e.toInt) } 

  val comp2: Check[Int] = catchWrong(comp1)(handle)
  
} */

