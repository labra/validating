package es.weso.validating

import cats.Functor
import cats.implicits._

/**
  * Represents Constraint errors
  */
abstract class ConstraintError[A] {
  def msg: String
}

/**
  * Constraint violation when there was an All constraint but some are not valid
  */
case class All_SomeNotValid[A](msg: String, notValids: Seq[ConstraintError[A]]) extends ConstraintError[A]

/**
  * Constraint violation when there was a Some constraint but no one is valid
  */
case class Some_NoOneValid[A](msg: String) extends ConstraintError[A]

/**
  * Constraint violation when there was a OneOf constraint and no one is valid
  */
case class OneOf_NoOneValid[A](msg: String) extends ConstraintError[A]

/**
  * Constraint violation when there was a OneOf constraint and more than one is valid
  */
case class OneOf_MoreThanOneValid[A]
(msg: String,
 valids: Seq[Checked[A, ConstraintReason, ConstraintError[A]]]) extends ConstraintError[A]

/**
  * Message based error
  */
case class MsgError[A](msg: String) extends ConstraintError[A]

object ConstraintError {
  implicit val cErrorFunctor: Functor[ConstraintError] = new Functor[ConstraintError] {
    override def map[A, B](ce: ConstraintError[A])(f: A => B): ConstraintError[B] = {
      ce match {
        case All_SomeNotValid(msg, cs) => {
          val fn: ConstraintError[A] => ConstraintError[B] = _.map(f)
          val csb: Seq[ConstraintError[B]] = cs.map(fn)
          All_SomeNotValid(msg, csb)
        }
        case Some_NoOneValid(msg) => Some_NoOneValid(msg)
        case OneOf_NoOneValid(msg) => OneOf_NoOneValid(msg)
        case OneOf_MoreThanOneValid(msg, vs) => {
          val vsb: Seq[Checked[B, ConstraintReason, ConstraintError[B]]] = {
            vs.map(c => {
              val vb: Checked[B, ConstraintReason, ConstraintError[A]] = c.mapValue(f)
              val vbes: Checked[B, ConstraintReason, ConstraintError[B]] = vb.mapErrors(_.map(f))
              vbes
            })
          }
          OneOf_MoreThanOneValid(msg, vsb)
        }
      case MsgError(msg) => MsgError(msg)
      case _ => throw new RuntimeException(s"map ConstraintError: Unknown constraint error type: $ce")
    }
   }
  }

}
