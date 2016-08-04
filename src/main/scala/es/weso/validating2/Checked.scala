package es.weso.validating2
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

trait Checked {
  type S // State 
  type E // Errors
  type C // Context

  // With this import we use list for non determinism 
  import cats.std.list._

  type ResultR[A] =  List[Xor[NonEmptyList[E],(A, S)]]
  
  def isOK[A](r: ResultR[A]): Boolean = !r.isEmpty  
  
  type Effects =  
    Reader[C,?] |:
    State[S,?] |:
    Validate[E, ?] |:
    Choose |: 
    Eval |:
    NoEffect

 type Check[A] = Eff[Effects,A]

 def runner[A](ctx: C, state: S, c: Check[A]) : ResultR[A] = 
   c.runReader(ctx).runState(state).runNel.runChoose.runEval.run
  
}