package es.weso.validating2 
import org.scalatest._
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._

class TypeCheckerTest extends FunSpec with Matchers with OptionValues with Checked {
  type Node = String
  type Edge = String
  type Graph = Map[Node, Set[(Edge,Node)]]
  type Shapes = Map[Label, Shape]
  type Label = String
  sealed abstract class Shape
  case class EmptyShape() extends Shape
  type Typing = Map[Node, Set[Label]]
  type Action = List[String]
  type ViolationError = String
  type S = (Typing,Action)
  type E = ViolationError
  type C = (Graph,Shapes)
  
  def graph(c: C): Graph = c._1
  def shape(c: C): Shapes = c._2
  
  def neigh(n: Node, g: Graph): Set[(Edge,Node)] = g(n)

  def addType(node: Node, label:Label, t: Typing): Typing = {
    t + (node -> Set(label))
  }

  def ok: Check[S] =
    getState
  
  def err(e: ViolationError): Check[S] = for {
    s <- get[Effects,S]
    _ <- wrong[Effects,ViolationError](e)
  } yield(s)
  
  def getGraph: Check[Graph] = for {
    ctx <- ask[Effects,C]
  } yield graph(ctx)
    
  def getShapes: Check[Shapes] = for {
    ctx <- ask[Effects,C]
  } yield shape(ctx)
  
  def getNeighbours(node: Node): Check[Set[(Edge,Node)]] = for {
    g <- getGraph
  } yield neigh(node,g)
  
  def getState: Check[S] = get[Effects,S]

  def check(node: Node, label: Label): Check[S] = for {
    ns <- getNeighbours(node)
    shape <- getShape(label)
    s <- matchNeighboursShape(ns,shape)
  } yield s
  
  def matchNeighboursShape(ns: Set[(Edge,Node)], shape: Shape) = shape match {
    case EmptyShape() => 
      if (ns.isEmpty) ok
      else err(s"Don't match $ns with emptyShape") 
  }
  
/*   def validateOption[R, E, A](option: Option[A], e: E)(implicit m: Validate[E, ?] |= R): Eff[R, A] =
    option.map(v => correct(v)).getOrElse(wrong(e)) */
  
  // TODO: improve this one to avoid the unsafe get...
  // Probably define another combinator...
  def getShape(label: Label): Check[Shape] = for {
    shapes <- getShapes
    _ <- validateOption[Effects, ViolationError, Shape](shapes.get(label),s"Shape not found for label $label") 
  } yield shapes.get(label).get
  
  describe("Simple Type Checker using the validating library") {
    it("Can check an empty shape") {
      val emptyShape = EmptyShape()
      val shapes: Shapes = Map("S" -> emptyShape)
      val graph: Graph  = Map("n1" -> Set(), "n2" -> Set(("x", "n1")))
      val ctx: C = (graph,shapes)
      val typing: S = (Map(),List())
      val result: ResultR[S] = runner(ctx, typing, check("n1", "S"))
      val ok: Boolean = isOK(result)
      ok should be(true)
    }

   
  }
}