package es.weso.validating

import cats.Functor
import cats.implicits._
import ConstraintReason._
import Checked._
import org.scalactic._

/**
 * A constraint can be seen as a function from a context and a value that returns a 
 * validated value
 * 
 * @tparam Context Type of context in which the constraint is evaluated
 * @tparam A Type of values that the constraint validates
 *
 */
sealed abstract class Constraint[Context, A] {
  def id: String 
  def validate: (A,Context) => Checked[A,ConstraintReason,ConstraintError[A]]
}

/**
   * Given a set of constraints, creates a constraint that is satisfied when all
   * the constraints are satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * 
   */
case class All[C,A](cs: Seq[Constraint[C,A]]) extends Constraint[C,A] {
  override def id = "All" 
  override def validate = (x,ctx) => {
    Constraint.all(cs.map(c => c.validate(x,ctx)))
  }
  override def toString: String = {
    s"$id[${cs.mkString(",")}]"
  }
}

/**
   * Given a set of constraints, creates a constraint that is satisfied when at least 
   * one of the constraints is satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * 
   */
case class SomeOf[C,A](cs: Seq[Constraint[C,A]]) extends Constraint[C,A] {
  override def id="SomeOf"
  override def validate = (x,ctx) => 
    Constraint.some(cs.map(c => c.validate(x,ctx)))
  override def toString: String = {
    s"$id[${cs.mkString(",")}]"
  }
}

  /**
   * Given a set of constraints, creates a constraint that is satisfied when one and only one of
   * the constraints is satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * 
   */
case class OneOf[C,A](cs: Seq[Constraint[C,A]]) extends Constraint[C,A] {
  override def id="OneOf"
  override def validate = (x,ctx) => 
    Constraint.oneOf(cs.map(c => c.validate(x,ctx)))
  override def toString: String = {
    s"$id[${cs.mkString(",")}]"
  }
}

  /**
   * Given two constraints, creates a constraint that is satisfied when both are satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * 
   */
case class AndConstraint[C,A]
  (c1: Constraint[C,A],
   c2: Constraint[C,A]) extends Constraint[C,A] {
  override def id="And"
  override def validate : (A,C) => Checked[A,ConstraintReason,ConstraintError[A]] = (x,ctx) => {
    lazy val v1 = c1.validate(x,ctx)
    lazy val v2 = c2.validate(x,ctx)
    Constraint.all(Seq(v1,v2))
  }
  override def toString: String = {
    s"$id[$c1,$c2]"
  }
}

  /**
   * Given two constraints, creates a constraint that is satisfied when one or the other are satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * 
   */
case class OrConstraint[C,A]
  (c1: Constraint[C,A],
   c2: Constraint[C,A]) extends Constraint[C,A] {
  override def id="Or"
  override def validate : (A,C) => Checked[A,ConstraintReason,ConstraintError[A]] = (x,ctx) => {
    lazy val v1 = c1.validate(x,ctx)
    lazy val v2 = c2.validate(x,ctx)
    Constraint.or(v1,v2)
  }
  override def toString: String = {
    s"$id[$c1,$c2]"
  }
}

  /**
   * Given two constraints, creates a constraint that is satisfied when one or the other, bot not both, are satisfied
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * 
   */
case class Xor[C,A](c1: Constraint[C,A],
                    c2: Constraint[C,A]) extends Constraint[C,A] {
  override def id="Xor"
  override def validate : (A,C) => Checked[A,ConstraintReason,ConstraintError[A]] = (x,ctx) => {
    lazy val v1 = c1.validate(x,ctx)
    lazy val v2 = c2.validate(x,ctx)
    Constraint.xor(v1,v2)
  }
  override def toString: String = {
    s"$id[$c1,$c2]"
  }
}

  /**
   * A single constraint which is not composed of other constraints 
   * 
   * @tparam C Context
   * @tparam A Type of Values to validate
   * 
   */
case class Single[C,A](
  validate: (A,C) => Checked[A,ConstraintReason,ConstraintError[A]]) extends Constraint[C,A] {
  override def id="Single"
  override def toString: String = "<single>"
}

object Constraint {

  /**
   * Creates a single constraint
    *
    * @tparam C Context
   * @tparam A Type of Values to validate
   * @param fn Validation function
   */
  def single[C,A](fn: (A,C) => Checked[A,ConstraintReason,ConstraintError[A]]): Constraint[C,A] =
    Single(fn)

  /**
   * Creates a validated value from a list of validated values that is satisfied when all are satisfied
    *
    * @tparam A Type of Values to validate
   * @param vs Sequence of validated values
   */
  def all[A](vs: Seq[Checked[A,ConstraintReason,ConstraintError[A]]]): Checked[A,ConstraintReason,ConstraintError[A]] = {
    def comb(r1: Response[A,ConstraintReason],
             r2: Response[A,ConstraintReason]): Response[A,ConstraintReason] = 
           Response(AllReason(Seq(r1.response,r2.response)))
    val zero : Checked[A,ConstraintReason,ConstraintError[A]] = Checked.okZero()
    def next(rest: Checked[A,ConstraintReason,ConstraintError[A]],
             current: Checked[A,ConstraintReason,ConstraintError[A]]): Checked[A,ConstraintReason,ConstraintError[A]] = {
      current.fold(rs1 => 
        rest.fold(rs2 => {
         val rs = rs1.combineWith(rs2,comb)
         Checked.oks(rs) 
        }
        , es => {
            err(All_SomeNotValid(s"Element not valid: $rest",es))
          }
        ),
        es1 => {
          err(All_SomeNotValid(s"Element not valid: $current", es1))
        }
      )
    }
    vs.foldLeft(zero)(next)  
  }
  
  /**
   * Creates a checked value from a list of validated values that is satisfied when at least one is satisfied
    *
    * @tparam A Type of Values to validate
   * @param vs Sequence of validated values
   */
  def some[A](vs: Seq[Checked[A,ConstraintReason,ConstraintError[A]]]): Checked[A,ConstraintReason,ConstraintError[A]] = {
    val zero : Checked[A,ConstraintReason,ConstraintError[A]] = {
      val e : ConstraintError[A] = Some_NoOneValid("No one valid")
      checkError(e)
    }
    def next(
        rest: Checked[A,ConstraintReason,ConstraintError[A]],
        current: Checked[A,ConstraintReason,ConstraintError[A]]): Checked[A,ConstraintReason,ConstraintError[A]] = {
       or(current,rest)
      }
     vs.foldLeft(zero)(next)      
    }

  /**
   * Creates a validated value from two validated values that is satisfied when at least one of them is satisfied
    *
    * @tparam A Type of Values to validate
   * @param v1 First validated value
   * @param v2 Second validated value
   */
  def or[A](v1: Checked[A,ConstraintReason,ConstraintError[A]],
            v2: Checked[A,ConstraintReason,ConstraintError[A]]): Checked[A,ConstraintReason,ConstraintError[A]] = {
    if (v1.isOK) {
      val rs1 = v1.reasons.get
      def f(r: Response[A,ConstraintReason]):Response[A,ConstraintReason] = Response(SomeReason(Seq(r.response)))
      Checked.oks(rs1.mapResponse(r => f(r)))
    } else {
      if (v2.isOK) {
         val rs2 = v2.reasons.get
         def f(r: Response[A,ConstraintReason]):Response[A,ConstraintReason] = Response(SomeReason(Seq(r.response)))
         Checked.oks(rs2.mapResponse(r => f(r)))
      } else {
         err(Some_NoOneValid(s"Both branches are not valid: $v1 and $v2"))
      }
    }
  }

  /**
   * Creates a validated value from a list of validated values that is satisfied when only one is satisfied
    *
    * @tparam A Type of Values to validate
   * @param vs Sequence of validated values
   */
  def oneOf[A](vs: Seq[Checked[A,ConstraintReason,ConstraintError[A]]]): Checked[A,ConstraintReason,ConstraintError[A]] = {
    val zero : Checked[A,ConstraintReason,ConstraintError[A]] = {
      val e: ConstraintError[A] = OneOf_NoOneValid("No one valid")
      checkError(e)
    }
    def next(rest: Checked[A,ConstraintReason,ConstraintError[A]],
             current: Checked[A,ConstraintReason,ConstraintError[A]]): Checked[A,ConstraintReason,ConstraintError[A]] = {
       xor(current,rest)
      }
     vs.foldLeft(zero)(next)      
    }
  
  /**
   * Creates a validated value from two validated values that is satisfied when only one is satisfied
    *
    * @tparam A Type of Values to validate
   * @param v1 First validated value
   * @param v2 Second validated value
   */
  def xor[A](v1: Checked[A,ConstraintReason,ConstraintError[A]],
             v2: Checked[A,ConstraintReason,ConstraintError[A]]): Checked[A,ConstraintReason,ConstraintError[A]] = {
     (v1.isOK,v2.isOK) match {
       case (true, true) => {
         val e : ConstraintError[A] = OneOf_MoreThanOneValid(s"Both branches are valid: $v1 and $v2", Seq(v1,v2))
         err(e)
       }
       case (true,false) => {
         val rs1 = v1.reasons.get
         def f(r: Response[A,ConstraintReason]):Response[A,ConstraintReason] = Response(OneOfReason(r.response))
         Checked.oks(rs1.mapResponse(r => f(r)))
       }

       case (false,true) => {
         val rs2 = v2.reasons.get
         def f(r: Response[A,ConstraintReason]):Response[A,ConstraintReason] = Response(OneOfReason(r.response))
         Checked.oks(rs2.mapResponse(r => f(r)))
       }

       case (false,false) => {
         val e: ConstraintError[A] = OneOf_NoOneValid(s"Both branches are not valid: $v1 and $v2")
         checkError(e)
       }
      }
  }


  /**
    * Checks that a value satisfies all of the conditions
    *
    * @tparam A type of value
    * @param x value to check
    * @param conds a sequence of conditions
    * @return the value if all the conditions are ok, otherwise, the accumulated errors
    */
  def checkValueAll[A]
    (x: A,
     conds: Seq[A => Checked[A,ConstraintReason,ConstraintError[A]]]):
     Checked[A,ConstraintReason,ConstraintError[A]] = {
    val cs = conds.map(c => c(x))
    Constraint.all(cs)
  }

  /**
    * Checks if a value satisfies at least one of the conditions
    *
    * <p> This combinator returns a checker for the value but doesn't keep track
    * about which of the conditions was satisfied
    *
    * @tparam A type of value
    * @param x value to check
    * @param conds sequence of conditions
    * @return a checker for value x that is ok if at least one of the conditions is satisfied
    */
  def checkSome[A]
    (x: A,
     conds: Seq[A => Checked[A,ConstraintReason,ConstraintError[A]]]
    ): Checked[A,ConstraintReason, ConstraintError[A]] = {
    val cs = conds.map(c => c(x))
    Constraint.some(cs)
  }

  def addSomeReason[A](nd: NDResponse[A,ConstraintReason]): NDResponse[A,ConstraintReason] = {
    nd.mapResponse(r => Response(ConstraintReason.someReason(Seq(r.response))))
  }

  def unsupported[Ctx,A,R[_]:Functor](msg:String):Constraint[Ctx,A] = {
    val r: Checked[A,ConstraintReason,ConstraintError[A]] = errString(msg)
    def fn(x: A,ctx :Ctx): Checked[A,ConstraintReason,ConstraintError[A]] = r
    single(fn)
  }

  def err[A](e: ConstraintError[A]): Checked[A,ConstraintReason,ConstraintError[A]] = {
    Checked.checkError(e)
  }

 /**
  * Creates a checked value which contains an error String
   *
   * @tparam A type of values (ignored)
  * @param msg error
  */
 def errString[A](msg:String): Checked[A,ConstraintReason,ConstraintError[A]] = {
   err(MsgError(msg))
 }


 def okSingle[A](x: A, msg:String): Checked[A,ConstraintReason,ConstraintError[A]] = {
   ok(singleReason(x,msg))
 }

  /**
    * Converts a boolean condition into a single constraint
    *
    * @param c boolean condition
    * @param msg msg that identifies the boolean condition
    * @tparam A type of values
    * @return a function that takes a value and returns a checked value
    */
  def cond[A](c : A => Boolean, msg: String): A => Checked[A,ConstraintReason,ConstraintError[A]] = { x =>
    if (c(x)) okSingle(x, s"$msg passed")
    else errString(s"$msg not passed")
  }

  def mapErrors[A,B,C]
    (checked: Checked[A,ConstraintReason,ConstraintError[B]],
     f: B => C): Checked[A,ConstraintReason,ConstraintError[C]] = {
    checked.mapErrors(ce => ce.map(f))
  }


}