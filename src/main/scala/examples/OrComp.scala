package examples 
import cats._, data._
import cats.implicits._

abstract class BacktrackingChecker {
  type Config
  type Env
  type Err
  type Log
  implicit val envMonoid: Monoid[Env]
  implicit val logMonoid: Monoid[Log]

type ReaderConfig[A] = Kleisli[Id, Config, A]
type ReaderEC[A] = Kleisli[ReaderConfig, Env, A]
type WriterEC[A] = WriterT[ReaderEC, Log, A] 
type Check[A] = EitherT[WriterEC, Err, A]

def run[A](c: Check[A])(config: Config)(env: Env = Monoid[Env].empty): (Log, Either[Err, A]) =
    c.value.run.run(env).run(config)
 
}