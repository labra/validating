package es.weso.validating
import cats._
import cats.syntax.all._

/**
 * An Explanation about why a constraint is satisfied
 * @tparam A type of value that is being validated
 */
sealed abstract class ConstraintReason[A]

/**
 * A Some Constraint is satisfied. 
 * @tparam A type of value that is being validated
 * @param cs set of constraint reasons that are satisfied
 */
case class SomeReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]

/**
 * A All Constraint is satisfied. 
 * @tparam A type of value that is being validated
 * @param cs constraint reasons that are satisfied
 */
case class AllReason[A](cs: Seq[ConstraintReason[A]]) extends ConstraintReason[A]

/**
 * A OneOf Constraint is satisfied. 
 * @tparam A type of value that is being validated
 * @param c constraint reason that is satisfied
 */
case class OneOfReason[A](c: ConstraintReason[A]) extends ConstraintReason[A]

/**
 * A Single Constraint is satisfied. 
 * @tparam A type of value that is being validated
 * @param x value that has been validated
 * @param msg explanation of validation
 */
case class SingleReason[A](x: A, msg: String) extends ConstraintReason[A]

object ConstraintReason {


/**
 * Construct a single reason  
 * @tparam A type of value that is being validated
 * @param x value that has been validated
 * @param msg explanation of validation
 */
def singleReason[A](x:A,msg:String): ConstraintReason[A] = 
    SingleReason(x,msg)
    
/**
 * Construct a All constraint reason  
 * @tparam A type of value that is being validated
 * @param cs set of constraint reasons
 */
def allReason[A](cs:Seq[ConstraintReason[A]]): ConstraintReason[A] = 
    AllReason(cs)

/**
 * Construct a Some constraint reason  
 * @tparam A type of value that is being validated
 * @param cs set of constraint reasons
 */
def someReason[A](cs:Seq[ConstraintReason[A]]): ConstraintReason[A] = 
    SomeReason(cs)

/**
 * Implicit value definition so ConstraintReason can be an Applicative instance    
 */
implicit val cReason = new Applicative[ConstraintReason] {
  
  override def map[A,B](fa: ConstraintReason[A])(f:A => B): ConstraintReason[B] = {
    fa match {
      case SomeReason(cs) => SomeReason(cs.map(c => this.map(c)(f)))
      case AllReason(cs) => AllReason(cs.map(c => this.map(c)(f)))
      case OneOfReason(c) => OneOfReason(this.map(c)(f))
      case SingleReason(x,msg) => SingleReason(f(x),msg)
    }
  }
  
  override def pure[A](x:A): ConstraintReason[A] = {
    SingleReason(x,"")
  }
  
  override def ap[A, B](fn: ConstraintReason[A => B])
        (fa: ConstraintReason[A]): ConstraintReason[B] = {
   fa match {
      case SomeReason(cs) => SomeReason(cs.map(c => this.ap(fn)(c)))
      case AllReason(cs) => AllReason(cs.map(c => this.ap(fn)(c)))
      case OneOfReason(c) => OneOfReason(this.ap(fn)(c))
      case SingleReason(x,msg1) => {
        fn match {
          case SingleReason(f,msg2) => SingleReason(f(x),msg2 ++ msg1)
          case SomeReason(cs) => SomeReason(cs.map(c => ap(c)(fa)))
          case AllReason(cs) => AllReason(cs.map(c => ap(c)(fa)))
          case OneOfReason(c) => OneOfReason(ap(c)(fa))
        }
      }
    } 
  }
  
  // TODO: I supposed that this should not need to be implemented
  // In principle it should be obtained from the others...
  // I took this definition from: https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Apply.scala
  override def product[A, B](fa: ConstraintReason[A],
      fb: ConstraintReason[B]): ConstraintReason[(A, B)] = 
    ap(map(fa)(a => (b: B) => (a, b)))(fb)
}

}
