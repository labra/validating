package examples
import cats._, data._
import org.atnos.eff._, all._ 
import org.atnos.eff.syntax.all._

object Backtracking {
  
//// With 3 effects, it fails when I use runNel at the beginning

type C = Fx.fx3[
  Writer[String,?],
  Reader[String,?],
  Validate[String,?]
  ]

type Check[A] = Eff[C,A]

val v3 : Check[Int] = pure(3)

val p : Check[Int] = catchWrong(v3)((e: String) => pure(4))

def run[A](c: Check[A]) = 
  c.runNel.runReader("Pepe").runWriter.run
}
