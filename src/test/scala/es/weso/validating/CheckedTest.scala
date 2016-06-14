package es.weso.validating
import Checked._
import NDResponse._
import org.scalatest._
import cats.implicits._

class CheckedTest 
 extends FunSpec with Matchers with OptionValues {
  
   type Explain[A] = Option[A]
   type E = Throwable
   type CheckedSingle = Checked[Int,Explain,E]
   type CheckedSeq = Checked[Seq[Int],Explain,E]

  describe("Checked") {
    
    it("Checks simple ok") {
      val x2: Checked[Int,List,Throwable] = ok(List(2))
      x2.isOK should be(true)
    }
    
    it("Checks simple error") {
      val e: Checked[Int,List,Throwable] = checkError(new Exception("Error"))
      e.isOK should be(false)
    }
    
    it("Converts an ok value into an error") {
      val v: Checked[Int,List,Throwable] = ok(List(2))
      val e = v.addError("Hi")
      e.isOK should be(false) 
      e.errors should contain only("Hi")
    }
    
    it("Accumulates several errors") {
      val v: Checked[Int,List,Throwable] = ok(List(2))
      val e = v.addErrors(Seq("Hi","man"))
      e.isOK should be(false) 
      e.errors should contain only("Hi", "man")
    }

    it("should be able to fold an ok value") {
      val v: Checked[Int,List,Throwable] = ok(List(2))
      val folded = v.fold(x => x.values.map(r => r.mapValue(_ + 1)), _ => 0)
      folded should be(Seq(Response(List(3))))
    }
    
    it("should be able to fold an errored value") {
      val v: Checked[Int,List,Throwable] = checkError(new Exception("Hi"))
      val folded = v.fold(x => x.values.map(r => r.mapValue(_ + 1)), _ => 0)
      folded should be(0)
    }

    it("should be able to merge a value with a list") {
      val v1: Checked[Int,List,Throwable] = ok(List(1))
      val v23: Checked[Seq[Int],List,Throwable] = ok(List(Seq(2,3)))
      val v = v1.merge(v23) 
      v.isOK should be(true)
      val expected: NDResponse[Seq[Int],List] = NDResponse(Seq(Response(List(Seq(1,2,3)))))
      val reasons = v.reasons.value
      reasons should be(expected)
    }

    it("should be able to merge an empty value with a list") {
      val v0: Checked[Int,List,Throwable] = okZero()
      val v23: Checked[Seq[Int],List,Throwable] = ok(List(Seq(2,3)))
      val v = v0.merge(v23) 
      v.isOK should be(true)
      val expected: NDResponse[Seq[Int],List] = NDResponse(Seq(Response(List(Seq(2,3)))))
      val reasons = v.reasons.value
      reasons should be(expected)
    }
    
    describe("all") {
      it("should be able to pass when one pass") {
        val v1: CheckedSingle = ok(Some(1))
        val v2: CheckedSingle = ok(Some(2))
        val vs : Seq[CheckedSingle] = Seq(v1,v2)
        val vall: CheckedSeq = checkAll(vs)
        vall.isOK should be(true)
        vall.errors should be(Seq())
        val reasons = vall.reasons.value
        val expected : NDResponse[Seq[Int],Explain] = 
          NDResponse(Seq(Response(Some(Seq(1,2)))))
        reasons should be(expected)
      }
      
      it("should fail when one fails") {
        val v1: CheckedSingle = checkError(new Exception("err"))
        val v2: CheckedSingle = ok(Some(2))
        val vs : Seq[CheckedSingle] = Seq(v1,v2)
        val vall: CheckedSeq = checkAll(vs)
        vall.isOK should be(false)
      }
    }

  }
   
  describe("Checked custom errors") {
    case class MyError[A](msg:String) extends ConstraintError[A]
    type CheckedCustom = Checked[Int,Explain,MyError[Int]]
    
    it("can raise a custom error") {
      val e: MyError[Int] = MyError("Hi")
      val v1 : Checked[Int,Explain,MyError[Int]] = Checked.checkError(e)
      v1.isOK should be(false)
    }
    it("can raise a custom error or return ok") {
      val e: MyError[Int] = MyError("Hi")
      val v1 : Checked[Int,Explain,MyError[Int]] =
        if (4 % 2 == 1) Checked.checkError(e)
        else {
          val x : Checked[Int,Explain,MyError[Int]] = ok(Some(2))
          x.mapValue(_ + 3)
        }
      v1.isOK should be(true)
    }
   it("can calculate all with custom errors") {
      val e: MyError[Int] = MyError("Hi")
      val v1 : Checked[Int,Explain,MyError[Int]] = ok(Some(1))
      val v2 : Checked[Int,Explain,MyError[Int]] = ok(Some(2))
      val vs : Checked[Seq[Int],Explain,MyError[Int]] = checkAll(Seq(v1,v2))
      vs.isOK should be(true)
    }

    
  }
}


