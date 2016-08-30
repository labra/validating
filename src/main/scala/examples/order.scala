package examples
import cats._, data._
import org.atnos.eff._, all._ 
import org.atnos.eff.syntax.all._

object ComputationOrder {
  
//// With 3 effects, it fails when I use runNel at the beginning

type C3 = Fx.fx3[State[String,?],Choose,Validate[String,?]]

type Check3[A] = Eff[C3,A]

val p3 : Check3[Int] = pure(3)

import cats.instances.list._

val r31 = p3.runChoose.runState("baz").runNel.run
val r32 = p3.runChoose.runState("baz").runNel.run
val r33 = p3.runNel.runChoose.runState("baz").run

//// With 2 effects, it works

type C2 = Fx.fx2[Choose,Validate[String,?]]

type Check2[A] = Eff[C2,A]

val p2 : Check2[Int] = pure(3)

import cats.instances.list._

val r21 = p2.runChoose.runNel.run
val r22 = p2.runNel.runChoose.run


//// With Writer instead of Reader, it fails when I use runNel at the beginning

type CW = Fx.fx3[Writer[String,?],Choose,Validate[String,?]]

type CheckW[A] = Eff[CW,A]

val pw : CheckW[Int] = pure(3)


val rw1 = pw.runChoose.runNel.runWriter.run
val rw2 = pw.runChoose.runWriter.runNel.run
val rw3 = pw.runNel.runChoose.runWriter.run

//// With Writer instead of Reader, it fails when I use runNel at the beginning

type CWR = Fx.fx4[Reader[String,?],Writer[String,?],Choose,Validate[String,?]]

type CheckWR[A] = Eff[CWR,A]

val pwr : CheckWR[Int] = pure(3)

val rwr1 = pwr.runChoose.runReader("pepe").runWriter.runNel.run
val rwr2 = pwr.runNel.runChoose.runReader("pepe").runWriter.run

}
