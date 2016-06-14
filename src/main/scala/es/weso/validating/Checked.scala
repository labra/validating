package es.weso.validating

import org.scalactic._
import cats.Functor
import cats.Applicative
import cats.implicits._

/**
 * Checked defines values that have been checked. A checked value can contain a non-deterministic response
  * or a list of errors.
  *
  * @tparam A type of values
  * @tparam R Explanation type of validation
  * @tparam E Error type
  */
case class Checked[A,R[_]:Functor,+E] (
    value: NDResponse[A,R] Or Every[E]){
  
import Checked._
  
 /**
   * @return `true` if the value has no errors
   */
  def isOK: Boolean = value.isGood

  
 /**
   * Fold a validated value into another value
   * @tparam V the result type 
   * @param ok function to apply to the value when it is OK
   * @param err function to apply to the sequence of errors
   * @return the result of applying either the ok function or the err function 
   */
  def fold[V](ok:NDResponse[A,R] => V, err: Seq[E] => V): V = {
     value.fold(ok,(es: Every[E]) => err(es.toSeq))
  }
  

  /**
   * Merge a validated value with a validated of a list of values
   */
  def merge[E1 >: E](vs: Checked[Seq[A],R,E1]): Checked[Seq[A],R,E1] = {
    fold(rs => 
        vs.fold(rss => oks(rs.merge(rss)),  
                es => errs(es)), 
          es => errs(es))
  }
  
  /**
   * Build a new Checked value by applying a function to the values and reasons
   * @tparam B the value type of the new Checked
   * @param f the function to apply to the current value and reason
   * @return a new Checked with the new value or the list of existing errors
   */
  def map[B, R1[_] >: R[_]:Functor](f: NDResponse[A,R] => NDResponse[B,R1]): Checked[B,R1,E] = {
    Checked(value.map(f))
  }

  def mapErrors[E1](f: E => E1): Checked[A,R,E1] = {
    val fn : Every[E] => Every[E1] = _.map(f(_))
    Checked(value.badMap(fn))
  }
  
  /**
   * Build a new Checked value by applying a function to the value. It lets the reasons untouched
   * @tparam B the value type of the new Checker
   * @param f the function to apply to the possible values
   * @return a new Checked with the new value or the list of existing errors
   */
  def mapValue[B, E1 >: E](f: A => B): Checked[B,R,E1] = {
    Checked(value.map(rs => rs.map(f)))
  }
  
 /**
   * Adds an Error to a Checker.
   * @param e error to add
   * @return If the value was ok, converts it into an error with the value `e`, 
   * 					otherwise, adds the error to the list of errors
   */
  def addError[E1 >: E](e:E1): Checked[A,R,E1] = {
    Checked(value.fold(
        _ => Bad(Every(e)),
        es => {
          val newEvery : Every[E1] = es :+ e 
          Bad(es :+ e)
        }
        ))
  }
  
  /**
   * Adds a list of errors to a Checker
   * @param es list of errors to add
   * @return a checker with the list of errors es
   */
  def addErrors[E1 >: E](es: Seq[E1]): Checked[A,R,E1] = {
    val zero : Checked[A,R,E1] = this
    es.foldRight(zero)((e,rest) => rest.addError(e))
  }
  
  /**
   * Returns the list of errors of this checker
   * <p> If the checker is ok, the list will be empty
   */
  def errors: Seq[E] = {
    this.fold(x => Seq(), es => es.seq)
  }
  
  /**
   * Combine two validated values when their values are ok
   * It validates if at least one of the alternatives is validated
   */
  def combineSome[E1 >: E](other: Checked[A,R,E1]): Checked[A,R,E1] = {
    fold(rs1 => 
      other.fold(
          rs2 => oks(rs1 ++ rs2), 
          _ => oks(rs1)), 
      _ => other
    )
  }

  /**
   * Combine two validated values when their values are ok
   * It validates if both of the alternatives are validated
   */
  def combineAll[E1 >: E](other: Checked[A,R,E1])
      : Checked[A,R,E1] = {
    fold(rs => 
      other.fold(
          os => oks(rs ++ os), 
          es => errs(es)), 
        es => 
          errs(es.toSeq)
    )
  }
  
  /**
   * Combine two validated values when one one of the values is ok
   * It validates if only one of the alternatives is validated
   */
  /*def combineOne[E1 >: E](other: Checked[A,R,E1]): Checked[A,R,E1] = {
    fold(rs1 => 
      other.fold(
        rs2 => 
          err(OneOfWithSeveralValid(rs1 ++ rs2)), 
        _ => 
          oks(rs1)), 
      es1 => 
        other.fold(
            rs => oks(rs),
            es2 => errs(es1 ++ es2)))
  } */
  
  def reasons: Option[NDResponse[A,R]] = {
    if (isOK) {
      Some(value.get)
    } else
      None
  }

}

object Checked{
  
  /**
   * Make a validated value initialized with an error
   * @tparam E type of errors
   * @param e error
   */
  def checkError[A,R[_]:Functor,E,E1 >: E](e: E): Checked[A,R,E1] = 
    Checked(Bad(Every(e)))


  /**
   * Make a Checked value initialized with a sequence of errors
   * @tparam E type of errors
   * @param es sequence of errors
   */
  def errs[A,R[_]:Functor,E](es: Seq[E]): Checked[A,R,E] = {
    if (es.isEmpty) throw new Exception("errors must not be empty")
    else Checked(Bad(Every.from(es).get))
  }

  /**
   * Make a validated value with an ok value
   * @tparam A type of values
   * @tparam R type of reasons
   * @param x value
   */
  def ok[A,R[_]:Applicative,E](x: R[A]): Checked[A,R,E] = 
    Checked(Good(NDResponse.single(x))) 

  /**
   * Make a validated value from a sequence of values/reasons
   * @tparam A type of values
   * @tparam R type of reasons
   * @param rs sequence of values/reasons
   */
  def oks[A,R[_]:Functor,E](rs: NDResponse[A,R]): Checked[A,R,Nothing] = 
    Checked(Good(rs))

  /**
   * Make a validated value empty
   * @tparam A type of values
   * @tparam R type of reasons
   */
  def okZero[A,R[_]:Applicative](): Checked[A,R,Nothing] = {
    val r : NDResponse[A,R] = NDResponse.initial
    Checked(Good(r)) 
  }
  
  
  def checkAll[A,R[_]:Applicative,E](vs: Seq[Checked[A,R,E]]): 
        Checked[Seq[A],R,E] = {
    val zero: Checked[Seq[A],R,E] = okZero()
    def next(v: Checked[A,R,E], 
        rest: Checked[Seq[A],R,E]): Checked[Seq[A],R,E] = {
      v.merge(rest)
    }
    vs.foldRight(zero)(next)
  } 
}


