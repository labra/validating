package es.weso.checking
import org.scalatest._
import cats._, data._
import cats.implicits._

class CheckerEffTest extends FunSpec with Matchers with OptionValues {

    import CheckerEffStr._
    
    describe(s"Checker Eff") {
      it("Should be able to return a value") {
        val c: Check[Int] = ok(2)
        runValue(c)(c0)(e0) should be(Right(2))
      }
      it("Should be able to return an error") {
        val msg = "Error"
        val e: Check[Int] = err(msg)
        runValue(e)(c0)(e0) should be(Left(msg))
      }
      it("Should be able to do an or...") {
        val c: Check[Int] = ok(2)
        val c3: Check[Int] = ok(3)
        val e: Check[Int] = err("Err")
        val e1: Check[Int] = err("Err1")
        runValue(orElse(c, e))(c0)(e0) should be(Right(2))
        runValue(orElse(e, c))(c0)(e0) should be(Right(2))
        runValue(orElse(c, c3))(c0)(e0) should be(Right(2))
        runValue(orElse(e, e1))(c0)(e0) should be(Left("Err1"))
      }
      it("Should be able to do checkSome...") {
        val c1: Check[Int] = ok(1)
        val c2: Check[Int] = ok(2)
        val c3: Check[Int] = ok(3)
        val e: Check[Int] = err("Err")
        val e1: Check[Int] = err("Err1")
        runValue(checkSome(List(c1, e), "No one"))(c0)(e0) should be(Right(1))
        runValue(checkSome(List(e, c2, e), "No one"))(c0)(e0) should be(Right(2))
        runValue(checkSome(List(e, e1), "No one"))(c0)(e0) should be(Left("No one"))
        runValue(checkSome(List(c1, c2), "No one"))(c0)(e0) should be(Right(1))
      }
      it("Should be able to run local") {
        def addEnv(name: String, value: Int): Env => Env =
          _.updated(name, value)

        val getX: Check[Option[Int]] = for {
          env <- getEnv
        } yield (env.get("x"))
        runValue(getX)(c0)(e0) should be(Right(None))
        runValue(local(addEnv("x", 1))(getX))(c0)(e0) should be(Right(Some(1)))

        val c: Check[Option[Int]] = local(addEnv("x", 1))(getX) >> getX
        runValue(c)(c0)(e0) should be(Right(None))
      }
      it("Should be able to collect a single log") {
        val x1: Check[Int] = for {
          _ <- logStr("L1")
        } yield 1
        val log = runCheck(x1)(c0)(e0)._2
        log should be(List("L1"))
      }
      it("Should be able to collect two logs") {
        val x1: Check[Int] = for {
          _ <- logStr("L1")
        } yield 1
        val x2: Check[Int] = for {
          _ <- logStr("L2")
        } yield 1
        val log = runCheck(x1 >> x2)(c0)(e0)._2
        log should be(List("L1", "L2"))
      } 
      it("Should be able to collect two logs with checkSome") {
        val x: Check[Int] = logStr("L1") >> ok(1)
        val e: Check[Int] = logStr("E") >> err("Err")
        runLog(checkSome(List(x, e), "NoOne"))(c0)(e0) should be(List("L1"))
        runLog(checkSome(List(e, x), "NoOne"))(c0)(e0) should be(List("E", "L1"))
        runLog(checkSome(List(e, e), "NoOne"))(c0)(e0) should be(List("E", "E"))
      }
      it("Should be able to execute cond for some successful computation") {
        lazy val x1: Check[Int] = logStr("x1") >> ok(1)
        lazy val x2: Check[Int] = logStr("x2") >> ok(2)
        lazy val e: Check[Int] = logStr("E") >> err("Err")
        lazy val c1 = cond(x1, (_: Int) => x2, _ => e)
        println("Run(c1):" + runCheck(c1)(c0)(e0))
        runValue(c1)(c0)(e0) should be(Right(2))
        runLog(c1)(c0)(e0) should be(List("x1", "x2"))
      }
      it("Should be able to execute cond for some fail computation") {
        lazy val x1: Check[Int] = logStr("x1") >> ok(1)
        lazy val x2: Check[Int] = logStr("x2") >> ok(2)
        lazy val e: Check[Int] = logStr("E") >> err("Err")
        lazy val c1 = cond(x1, (_: Int) => e, _ => x2)
        println("Run(c1):" + runCheck(c1)(c0)(e0))
        runValue(c1)(c0)(e0) should be(Left("Err"))
        runLog(c1)(c0)(e0) should be(List("x1", "E"))
      } 
}
}

