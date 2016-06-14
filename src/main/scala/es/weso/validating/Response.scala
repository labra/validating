package es.weso.validating

import cats._
import cats.implicits._

/**
 * Defines a validation response
 * @tparam A type of values that have been validated
 * @tparam R container of the response (it must be an instance of applicative)
 * @param response response value
 */
case class Response[A,R[_]:Applicative](response: R[A]) {
  
  /**
   * Applies a function to the response value
   * @tparam B new type of response value
   * @param f function that is applied
   */
  def mapValue[B](f: A => B): Response[B, R] = {
    Response(response = implicitly[Functor[R]].map(response)(f))  
  }
  
  /**
   * Merges a sequence of responses with the current response
   * @param responses a response that contains a sequence of values
   * @return a response that contains a sequence of values formed by appending the current value to the sequence of values 
   */
  def merge(responses: Response[Seq[A],R]): Response[Seq[A],R] = {
    val r : R[A] = response
    val rs : R[Seq[A]] = responses.response
    def add(p:(A,Seq[A])):Seq[A] = p._1 +: p._2
    val p : R[Seq[A]] = r.product(rs).map(add)
    Response(p)
  }
  
  override def toString:String = {
    s"Response[$response]"
  }
}

object Response {

  /**
   * Add two sequence of response values
   * @tparam A type of values
   * @tparam R type of response containers 
   * @param rs1 first sequence of response values
   * @param rs2 second sequence of response values
   * @return sequence of response values formed by concatenating both rs1 and rs2 
   */
  def add[A,R[_]:Applicative](
      rs1: Response[Seq[A],R], rs2: Response[Seq[A],R]): Response[Seq[A],R] = {
    val r1 : R[Seq[A]] = rs1.response
    val r2 : R[Seq[A]] = rs2.response
    def add(p:(Seq[A],Seq[A])):Seq[A] = p._1 ++ p._2
    val p : R[Seq[A]] = r1.product(r2).map(add)
    Response(p)
  }
}
