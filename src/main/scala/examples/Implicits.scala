package example
import cats._, data._
import cats.implicits._

abstract class CheckerWriter {
  type Config 
  type Err 
  type Log 
  implicit val logMonoid: Monoid[Log]

  type ReaderConfig[A] = Kleisli[Id, Config, A]
  type WriterEC[A] = WriterT[ReaderConfig, Log, A]
  type Check[A] = EitherT[WriterEC, Err, A]
  
  def ok[A](x: A): Check[A] = EitherT.pure[WriterEC,Err,A](x)
  def run[A](c: Check[A], config: Config): (Log,Either[Err,A]) =  
      c.value.run.run(config)
}

object MyCheckWriter extends CheckerWriter {
  type Config = String
  type Err = String
  type Log = String
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log) = l1 + "\n" + l2
    def empty = ""
  }
}

object Test1 {
 import MyCheckWriter._
 val c1: Check[Int] = ok(3)

 // val c2 : Check[Int] = c1.flatMap(v => ok(v + 1)) // Doesn't find implicit Monad for WriterEC
 
}

// The same as before using StateT
abstract class CheckerState {
  type Config 
  type Err 
  type Log 
  implicit val logMonoid: Monoid[Log]

  type ReaderConfig[A] = Kleisli[Id, Config, A]
  type StateEC[A] = StateT[ReaderConfig, Log, A]
  type Check[A] = EitherT[StateEC, Err, A]
  
  def ok[A](x: A): Check[A] = EitherT.pure[StateEC,Err,A](x)
  def run[A](c: Check[A], l: Log, config: Config): (Log,Either[Err,A]) =  
      c.value.run(l).run(config)
}

object MyCheckState extends CheckerState {
  type Config = String
  type Err = String
  type Log = String
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log) = l1 + "\n" + l2
    def empty = ""
  }
}

object Test2 {
 import MyCheckState._
 val c1: Check[Int] = ok(3)
 val c2 : Check[Int] = c1.flatMap(v => ok(v + 1)) 
 
}

