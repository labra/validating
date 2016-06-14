package es.weso.validating

import cats._
import cats.data.NonEmptyList
import cats.implicits._

/**
 * Represents a non-deterministic response which can contain several responses
 * @tparam A Type of values
 * @tparam R Response container
 * @param values sequence of deterministic responses
 */
case class NDResponse[A, R[_]: Applicative] private (
    values: Seq[Response[A, R]]) {

  /**
   * Merge a NDResponse that can contain a list of values with this response
   * @param rs a response which contains a sequence of values
   * @return a response which contains the sequence of values formed by appending this response to those values
   */
  def merge(rs: NDResponse[Seq[A], R]): NDResponse[Seq[A], R] = {
    if (values.isEmpty) rs
    else {
     val rss = values.map(ra => NDResponse.add(ra,rs))
     NDResponse.flatten(rss)
    }
  }

  /**
   * Extend the possible values with the values of another non-deterministic response
   * @param other the NDResponse whose values will be extended
   */
  def ++(other: NDResponse[A, R]): NDResponse[A, R] = {
    NDResponse(values  ++ other.values)
  }

  /**
   * Apply a function 
   * @tparam B type of values
   * @param f function to apply
   */
  def map[B](f: A => B): NDResponse[B, R] = {
    NDResponse(values.map(r => r.mapValue(f)))
  }

  /**
   * Combine with another NDResponse applying a function to each value
   * @param other other NDResponse
   * @param f combination function
   */
  def combineWith(
    other: NDResponse[A, R],
    f: (Response[A, R], Response[A, R]) => Response[A, R]): NDResponse[A, R] = {
    if (values.isEmpty) other
    else if (other.values.isEmpty) this
    else {
      val rs = for {
        v1 <- values;
        v2 <- other.values
      } yield f(v1, v2)
      NDResponse(rs)
    }
  }

  /**
   * Apply a function to each of the possible responses
   * @param f function to apply
   */
  def mapResponse(f: Response[A, R] => Response[A, R]): NDResponse[A, R] = {
    NDResponse(values.map(f))
  }

  override def toString: String = {
    s"NDResponse[${values.toString}]"
  }
}

object NDResponse {

  /**
   * Create a single NDResponse from a Response
   */
  def single[A, R[_]: Applicative](r: Response[A,R]): NDResponse[A, R] = {
    NDResponse(Seq(r))
  }
  
  /**
   * Create a single NDResponse from a Response
   */
  def single[A, R[_]: Applicative](r: R[A]): NDResponse[A, R] = {
    NDResponse(Seq(Response(r)))
  }

  /**
   * This function may be removed because we should not create blank NDResponse's
   * TODO: Change the code to be a non-empty list instead of a sequence
   */
  def initial[A, R[_]: Applicative]: NDResponse[A, R] = NDResponse(Seq())
  
  /**
   * 
   */
  def add[A,R[_]:Applicative](
        ra: Response[A, R], 
        rsa: NDResponse[Seq[A], R]): NDResponse[Seq[A], R] = {
    if (rsa.values.isEmpty) {
      val rs: Response[Seq[A],R] = ra.mapValue(x => Seq(x))
      single(rs)
    } else {
      NDResponse(rsa.values.map(rs => ra.merge(rs)))
    }
  }
  
  /**
   * Given a sequence of NDResponse's create a NDResponse concatenating their values 
   */
  def flatten[A,R[_]:Applicative](rss: Seq[NDResponse[A,R]]): NDResponse[A,R] = {
    def next(x: NDResponse[A,R], r: NDResponse[A,R]): NDResponse[A,R] = x ++ r
    rss.reduceLeft(next)
  }
}
