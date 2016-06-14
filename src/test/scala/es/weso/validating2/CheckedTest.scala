package es.weso.validating2
import org.scalatest._
import cats.implicits._

class CheckedTest
  extends FunSpec with Matchers with OptionValues {

  type CheckedSingle = Checked[Int]
  type CheckedSeq = Checked[Seq[Int]]

  describe("Checked") {

/*    it("Checks simple ok") {
      val x2: Checked[Int] = Checked.ok(2,"2 checked")
      x2.isOK should be(true)
    } */


  }
}


