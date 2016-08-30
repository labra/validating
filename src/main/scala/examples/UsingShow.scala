package examples
import cats._
import cats.data._
import cats.implicits._

abstract class UsingShow1 {
  type Value 
  implicit def showValue: Show[Value]
  
  def showValues(vs: List[Value]): String = 
    vs.map(showValue.show(_)).mkString
}

class UsingShow2[Value: Show] {
  
  def showValues(vs: List[Value]): String = 
    vs.map(implicitly[Show[Value]].show(_)).mkString
  
}

class Testing {
  class Check extends UsingShow1 {
    type Value = String
    override implicit def showValue: Show[String] = new Show[String] {
      def show(s: String) = s
    }
    def myShow(vs: List[Value]) = showValues(vs)
    
  }
}