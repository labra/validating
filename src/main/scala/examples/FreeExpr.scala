package examples
import cats._
import cats.data._
import cats.free._
import cats.free.Free._
import cats.implicits._
import scala.language.higherKinds

object FreeExpr {

  sealed trait ValueF[A]
  case class IntValue(n: Int) extends ValueF[Int]

  class Value[F[_]](implicit I: Inject[ValueF, F]) {
    def intVal(n:Int): Free[F,Int] = inject[ValueF,F](IntValue(n))
  }

  object Value {
    implicit def value[F[_]](implicit I: Inject[ValueF,F]): Value[F] =
      new Value[F]
  }

  sealed trait ArithF[A]
  case class Add[A](x: A, y: A, ev: Semigroup[A]) extends ArithF[A]

  class Arith[F[_]](implicit I: Inject[ArithF, F]) {
   def add[A](x: A, y: A, ev: Semigroup[A]) : Free[F,A] =
     Free.inject[ArithF,F](Add(x,y,ev))
  }

  object Arith {
    implicit def arith[F[_]](implicit I: Inject[ArithF,F]): Arith[F] =
      new Arith[F]
  }

  type Expr[A] = Coproduct[ArithF, ValueF, A]
  type Result[A] = Id[A]

  object ValueId extends (ValueF ~> Result) {
    def apply[A](fa: ValueF[A]) = fa match {
      case IntValue(n) => Monad[Id].pure(n)
    }
  }

  object ArithId  extends (ArithF ~> Result) {
    def apply[A](fa: ArithF[A]) = fa match {
      case Add(x,y,ev) => ev.combine(x,y)
    }
  }


  val interpreter: Expr ~> Result = ArithId or ValueId

  def expr1(implicit value : Value[Expr],
                     arith : Arith[Expr]): Free[Expr, Int] = {
    import value._, arith._
    val ev = implicitly[Semigroup[Int]]
    for {
      n <- intVal(2)
      m <- add(n, n, ev)
    } yield m
   }

  lazy val run1 = expr1.foldMap(interpreter)

  def expr2(implicit value : Value[Expr],
                     arith : Arith[Expr]): Free[Expr, String] = {
    import value._, arith._
    val ev = implicitly[Semigroup[String]]
    for {
      n <- intVal(2)
      m <- add(n.toString, n.toString, ev)
    } yield m
   }

   lazy val run2 = expr2.foldMap(interpreter)

}
