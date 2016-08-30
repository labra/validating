package es.weso.shacl
import cats._, data._
import cats.implicits._

object liftReader1 {
  type Err
  type Env
  type Config 
  type ReaderEnv[A] = Kleisli[Id,Env,A]
  type ReaderEC[A] = Kleisli[ReaderEnv,Config,A]
  type WriterEC[A] = WriterT[ReaderEC,String,A]
  type EitherEC[A] = EitherT[WriterEC,Err,A]
  
  def getEnv: EitherEC[Env] = {
   Kleisli.ask[Id, Env].
   liftT[λ[(F[_], A) => Kleisli[F, Config, A]]].
   liftT[λ[(F[_], A) => WriterT[F, String, A]]].
   liftT[λ[(F[_], A) => EitherT[F, Err, A]]]
  }
  
  def getConfig: EitherEC[Config] = {
   Kleisli.ask[ReaderEnv, Config].
   liftT[λ[(F[_], A) => WriterT[F, String, A]]].
   liftT[λ[(F[_], A) => EitherT[F, Err, A]]]
  }

}
