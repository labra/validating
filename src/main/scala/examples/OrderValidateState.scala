package examples
import cats._, data._
import org.atnos.eff._, all._ 
import org.atnos.eff.syntax.all._

object OrderValidateState {

type Stack = Fx.fx4[
  Reader[Int, ?], 
  Writer[String, ?], 
  Validate[String,?],
  Eval]
  
val program: Eff[Stack, Int] = for {
  // get the configuration
  n <- ask[Stack, Int]

  // log the current configuration value
  _ <- tell[Stack, String]("the required power is "+n)

  // compute the nth power of 2
  a <- delay[Stack, Int](math.pow(2, n.toDouble).toInt)

  // log the result
  _ <- tell[Stack, String]("the result is "+a)
} yield a

// run the action with all the interpreters
// each interpreter running one effect
lazy val run0 = program.runReader(6).runWriter.runNel.runEval.run

}
