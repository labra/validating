package examples
import cats._, data._
import org.atnos.eff._, all._ 
import org.atnos.eff.syntax.all._

object Eff {

type Stack = Reader[Int, ?] |: Writer[String, ?] |: Eval |: NoEffect
  
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
lazy val run0 = program.runReader(6).runWriter.runEval.run

type Stack2 = Validate[String,?] |: Reader[Int, ?] |: Writer[String, ?] |: Eval |: NoEffect

val program2: Eff[Stack2, Int] = for {
  n <- ask[Stack2, Int]
  _ <- tell[Stack2, String]("the required power is "+n)
  a <- delay[Stack2, Int](math.pow(2, n.toDouble).toInt)
  _ <- tell[Stack2, String]("the result is "+a)
} yield a

lazy val run2 = program2.runNel.runReader(4).runWriter.runEval.run

}
