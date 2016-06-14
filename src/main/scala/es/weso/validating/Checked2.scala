package es.weso.validating

import org.scalactic._
import cats.Functor
import scala.language.higherKinds
import cats.Applicative
import cats.implicits._
import cats.data._

/**
  * Checked defines values that have been checked. A checked value can contain a non-deterministic response
  * or a list of errors.
  *
  * @tparam A type of values
  * @tparam R Explanation type of validation
  * @tparam E Error type
  */
/*case class Checked2[A,R[_]:Functor,+E[_]] (
   value: NDResponse[A,R] Or Every[E[A]]) {
}
object Checked2 {

  def errs[A,R[_]:Functor,E[_]](es: Seq[E[A]]): Checked[A,R,E] = {
    if (es.isEmpty) throw new Exception("errors must not be empty")
    else {
      val ers : Option[Every[E[A]]] = Every.from(es)
      val v : Checked[A,R,E] = ???
      Checked2(Bad(ers.get))
      ???
    }
  }

}
*/