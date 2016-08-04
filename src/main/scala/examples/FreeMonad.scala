package examples
import cats.free.Free
import cats.free.Free._
import cats.arrow.NaturalTransformation
import cats.{Id, Eval, ~>}
import scala.collection.mutable
import cats.data.State
import cats.data.{Xor, Coproduct}
import cats.free.{Inject, Free}
import cats.{Id, ~>}
import scala.collection.mutable.ListBuffer

object FreeMonad {

sealed trait KVStoreA[A]
case class Put[T](key: String, value: T) extends KVStoreA[Unit]
case class Get[T](key: String) extends KVStoreA[Option[T]]
case class Delete(key: String) extends KVStoreA[Unit]
case class Cond[A](c: Boolean,
    ifTrue:  Free[KVStoreA,A],
    ifFalse: Free[KVStoreA,A]) extends KVStoreA[A]

type KVStore[A] = Free[KVStoreA, A]

// Put returns nothing (i.e. Unit).
def put[T](key: String, value: T): KVStore[Unit] =
  liftF[KVStoreA, Unit](Put[T](key, value))

// Get returns a T value.
def get[T](key: String): KVStore[Option[T]] =
  liftF[KVStoreA, Option[T]](Get[T](key))

// Delete returns nothing (i.e. Unit).
def delete(key: String): KVStore[Unit] =
  liftF(Delete(key))

// Update composes get and set, and returns nothing.
def update[T](key: String, f: T => T): KVStore[Unit] =
  for {
    vMaybe <- get[T](key)
    _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
  } yield ()

def cond[A](c: Boolean, ifTrue: => KVStore[A], ifFalse: => KVStore[A]) : KVStore[A] = {
    liftF(Cond(c,ifTrue,ifFalse))
  }


def program: KVStore[Unit] =
  for {
    _ <- put("wild-cats", 2)
    _ <- update[Int]("wild-cats", (_ + 12))
    _ <- put("tame-cats", 5)
    n <- get[Int]("wild-cats")
    _ <- cond(n.getOrElse(0) > 3,
                update[Int]("wild-cats", (_ * 12)),
                Free.pure(()))
    _ <- delete("tame-cats")
  } yield ()

// the program will crash if a key is not found,
// or if a type is incorrectly specified.
def impureCompiler: KVStoreA ~> Id  =
  new (KVStoreA ~> Id) {

    // a very simple (and imprecise) key-value store
    val kvs = mutable.Map.empty[String, Any]

    def apply[A](fa: KVStoreA[A]): Id[A] =
      fa match {
        case Put(key, value) =>
          println(s"put($key, $value)")
          kvs(key) = value
          ()
        case Get(key) =>
          println(s"get($key)")
          kvs.get(key).map(_.asInstanceOf[A])
        case Delete(key) =>
          println(s"delete($key)")
          kvs.remove(key)
          ()
        case Cond(c, ifThen, ifFalse) =>
          println(s"cond($c, ...)")
          if (c) ifThen.foldMap(impureCompiler)
          else ifFalse.foldMap(impureCompiler)
      }
  }

lazy val run0 = program.foldMap(impureCompiler)

// Pure compiler...State transformer...

type KVStoreState[A] = State[Map[String, Any], A]

val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
  def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
    fa match {
      case Put(key, value) =>
        State.modify(_.updated(key, value))
      case Get(key) =>
        State.inspect(_.get(key).map(_.asInstanceOf[A]))
      case Delete(key) => State.modify(_ - key)
      case Cond(c, ifThen, ifFalse) =>
          println(s"cond($c, ...)")
          if (c) ifThen.foldMap(pureCompiler)
          else ifFalse.foldMap(pureCompiler)
    }
}

lazy val run1 = program.foldMap(pureCompiler).run(Map.empty).value

/////////////////////////////////////////////////////////////////////////////////////////

/* Handles user interaction */
sealed trait Interact[A]
case class Ask(prompt: String) extends Interact[String]
case class Tell(msg: String) extends Interact[Unit]

/* Represents persistence operations */
sealed trait DataOp[A]
case class AddCat(a: String) extends DataOp[Unit]
case class GetAllCats() extends DataOp[List[String]]

type CatsApp[A] = Coproduct[DataOp, Interact, A]

class Interacts[F[_]](implicit I: Inject[Interact, F]) {
  def tell(msg: String): Free[F, Unit] = inject[Interact, F](Tell(msg))
  def ask(prompt: String): Free[F, String] = inject[Interact, F](Ask(prompt))
}

object Interacts {
  implicit def interacts[F[_]](implicit I: Inject[Interact, F]): Interacts[F] = new Interacts[F]
}

class KVStoreDef[F[_]](implicit I: Inject[KVStoreA, F]) {
  def putStore[T](key: String, value: T): Free[F, Unit] =
    Free.inject[KVStoreA, F](Put(key,value))
  def getStore[T](key: String): Free[F, Option[T]] =
    Free.inject[KVStoreA, F](Get(key))
  def delete(key: String): Free[F,Unit] =
    Free.inject(Delete(key))
  def update[T](key: String, f: T => T): Free[F,Unit] =
   for {
    vMaybe <- getStore[T](key)
    _ <- vMaybe.map(v => putStore[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()
}

object KVStoreDef {
  implicit def kvStoreDef[F[_]](implicit I: Inject[KVStoreA, F]): KVStoreDef[F] = new KVStoreDef[F]
}

class DataSource[F[_]](implicit I: Inject[DataOp, F]) {
  def addCat(a: String): Free[F, Unit] = Free.inject[DataOp, F](AddCat(a))
  def getAllCats: Free[F, List[String]] = Free.inject[DataOp, F](GetAllCats())
}

object DataSource {
  implicit def dataSource[F[_]](implicit I: Inject[DataOp, F]): DataSource[F] = new DataSource[F]
}

def program2(implicit I : Interacts[CatsApp], D : DataSource[CatsApp]): Free[CatsApp, Unit] = {

  import I._, D._

  for {
    cat1 <- ask("Name of first cat?")
    _ <- addCat(cat1)
    cat2 <- ask("Name of second cat?")
    _ <- addCat(cat2)
    cats <- getAllCats
    _ <- tell(cats.toString)
  } yield ()
}

type CatsApp2[A] = Coproduct[KVStoreA, Interact, A]

def program3(
    implicit I : Interacts[CatsApp2],
             S : KVStoreDef[CatsApp2]): Free[CatsApp2, Unit] = {

  import I._, S._

  for {
    cat1 <- ask("Name of first cat?")
    _ <- putStore("cat1",cat1)
    cat2 <- ask("Name of second cat?")
    _ <- putStore("cat2",cat2)
    cat <- getStore[String]("cat1")
    _ <- tell(cat.toString)
  } yield ()
}

object ConsoleCatsInterpreter extends (Interact ~> Id) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      readLine()
    case Tell(msg) =>
      println(msg)
  }
}

object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {

  private[this] val memDataSet = new ListBuffer[String]

  def apply[A](fa: DataOp[A]) = fa match {
    case AddCat(a) => memDataSet.append(a); ()
    case GetAllCats() => memDataSet.toList
  }
}

object KVStoreInterpreter extends (DataOp ~> Id) {

  private[this] val memDataSet = new ListBuffer[String]

  def apply[A](fa: DataOp[A]) = fa match {
    case AddCat(a) => memDataSet.append(a); ()
    case GetAllCats() => memDataSet.toList
  }
}

object ConsoleCatsInterpreter2 extends (Interact ~> KVStoreState) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      val str = readLine
      State.pure(str)
    case Tell(msg) =>
      println(msg)
      State.pure()
  }
}

val interpreter: CatsApp ~> Id = InMemoryDatasourceInterpreter or ConsoleCatsInterpreter

lazy val run2 = program2.foldMap(interpreter)

val interpreter3: CatsApp2 ~> Id = impureCompiler or ConsoleCatsInterpreter

lazy val run3 = program3.foldMap(interpreter3)

val interpreter4: CatsApp2 ~> KVStoreState = pureCompiler or ConsoleCatsInterpreter2

lazy val run4 = program3.foldMap(interpreter4).run(Map.empty).value

}
