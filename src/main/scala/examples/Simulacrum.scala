package examples
import simulacrum._
import cats._, data._
import implicits._

/*@typeclass(excludeParents = List("cats.kernel.Monoid"))
trait CanLog[A] extends Monoid[A] {
  def log(x: String): A
}

trait CanLog[A] {
  def log(x: String): A
}

abstract class ATest {
  import CanLog.ops._
  type Log
  implicit val logCanLog: CanLog[Log]
  implicit val logMonoid: Monoid[Log]
  
  def checkLog(msg: String): Log = CanLog[Log].log(msg)
}

class OTest extends ATest {
  type Log = String
  implicit val logCanLog = new CanLog[Log] {
    def log(x: String) = "log: " + x
  }
  implicit val logMonoid = new Monoid[Log] {
    def combine(x: String, y: String) = x + y
    def empty = ""
  }
}

object Test {
  import CanLog.ops._
  val t = new OTest
  
  val r = t.checkLog("pepe")
}

*/