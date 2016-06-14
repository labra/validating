package es.weso.validating

import cats.implicits._
import org.scalatest._
import ConstraintError._

class ConstraintErrorTest extends FunSpec with Matchers with OptionValues {

  describe("ConstraintError") {
    println("Constraint error...")
    val m : ConstraintError[Int] = MsgError("example")
    println(s"Constraint error $m...")
    val ce : ConstraintError[Int] = All_SomeNotValid("all", Seq(m))
    println(s"Constraint error $ce...")
    val cem : ConstraintError[Double]= ce.map(n => n.toDouble)
    println(s"Constraint error $cem...")
    cem.msg should be("all")
  }

}
