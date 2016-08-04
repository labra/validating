package examples
import cats._, data._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

object NodeTyper {

type Label = String
type Node = String
type Graph = Map[Node, Set[Node]]
type Shape = Seq[Constraint]
type Constraint = String
type Shapes = Map[Label, Shape]
type Context = (Graph, Shapes)
type Typing = Map[Label, Set[Label]]
type Violation = String
type Action = List[String]

type Result = Reader[Context,?] |:
              State[Typing, ?] |:
              Validate[Violation, ?] |:
              Eval |: 
              NoEffect

def addType(node: Node, label: Label, typing: Typing): Typing =
  typing + (node -> Set(label))

def checker(n:Node,l :Label): Eff[Result, Typing] = for {
  ctx <- ask[Result, Context]
  typing <- get[Result,Typing]
  _ <- put[Result, Typing](addType(n,l,typing))

} yield typing

val context0 : Context = (Map(),Map())
val typing0 : Typing = Map()
val action0 : Action = List()
val runner0 = checker("x","S").runReader(context0).runState(typing0).runNel.runEval.run

type R1 = Reader[Context,?] |:
          State[Action,?] |:
          State[Typing, ?] |:
          Validate[Violation, ?] |:
          Choose |: 
          Eval |:
          NoEffect
          
def check(v: Int): Eff[R1,String] = 
  v match {
    case 0 => zero[R1,String]
    case 1 => for {
      m <- chooseFrom[R1,Int](List(3,4))
      n <- chooseFrom[R1,String](List("a","b"))
    } yield m + n
    case 2 => pure[R1,String]("Two")
    case 3 => for {
      ctx <- ask[R1,Context]
    } yield s"Three. Context: $ctx" 
    case 4 => for {
      _ <- wrong[R1,String]("error with value 4")
      _ <- wrong[R1,String]("another error with value 4")
    } yield "..."
  }


import cats.std.list._

type ResultR[A] =  List[Xor[NonEmptyList[Violation],((A, Action), Typing)]]
def runner1(n:Int): ResultR[String] = 
  check(n).runReader(context0).runState(action0).runState(typing0).runNel.runChoose.runEval.run


}
