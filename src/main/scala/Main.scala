import cats.Monad
import cats.data.ReaderT
import cats.effect._
import cats.mtl._
import cats.implicits._
import cats.mtl.implicits._

object Main extends IOApp {

  final case class Env(print: String => Unit)

  type AppConfig[F[_]] = ApplicativeAsk[F, Env]

  def program[F[_]: Monad: AppConfig]: F[Unit] = for {
    env <- ApplicativeAsk.ask[F, Env]
  } yield {
    env.print("Hello, World!")
  }

  val interpreter = program[ReaderT[IO, Env, ?]]

  def run(args: List[String]): IO[ExitCode] = {
    val env = Env(print = println)
    interpreter.run(env).as(ExitCode.Success)
  }
}