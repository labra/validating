package es.weso.validating2

import cats.data._
import simulacrum._

import cats.data.Validated._

import es.weso.validating.NDResponse

trait Checked[A] {
  def isOK: Boolean
  def errors: Seq[CheckError[A]]
  def okReasons: Seq[Reason[A]]
  def addError(e: CheckError[A]): Checked[A]
  def ok(x:A, msg: String): Checked[A]
}

trait CheckError[A] {
  def value: A
  def errorCause: String
}

trait Reason[A] {
  def value: A
  def reason: String
  def reason(value: A, msg: String): Reason[A]
}

case class CheckedImpl[A] private (value: Validated[NonEmptyStream[CheckError[A]], NonEmptyStream[Reason[A]]])
 extends Checked[A] {
  def addError(e: CheckError[A]): CheckedImpl[A] = ???
  def isOK = value.isValid
  def errors = value.fold(es => es.head +: es.tail, _ => Stream())
  def okReasons = value.fold(es => Stream(), vs => vs.head +: vs.tail)
  def ok(x:A, msg:String) = ??? // valid(reason(x,msg))
}

case class ReasonImpl[A](value: A, reason: String) extends Reason[A] {
  def reason(value: A, reason: String): Reason[A] = ReasonImpl(value,reason)
}

case class CheckErrorImpl[A](value:A, errorCause: String) extends CheckError[A]


object Checked {

  def ok[A](x:A, msg: String): Checked[A] = ??? //

}