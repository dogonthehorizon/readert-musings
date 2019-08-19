import cats.Monad
import cats.data.ReaderT
import cats.effect._
import cats.mtl._
import cats.implicits._
import cats.mtl.implicits._

import scala.language.higherKinds

object Main extends IOApp {

  /** Newtype for strings that represent URLs. */
  final case class Url(value: String) extends AnyVal

  def run(args: List[String]): IO[ExitCode] = {
    args.headOption match {
      case Some(website) =>

        val env = Env(
          print = ioPrint,
          ping = pingWebsite,
          // TODO you might want to do some validation here
          websiteToPing = Url(website)
        )

        interpreter.run(env).as(ExitCode.Success)

      case None =>
        IO(System.err.println("Usage: isUp website")).as(ExitCode.Error)
    }
  }

  val interpreter = program[ReaderT[IO, Env, ?]]

  /** Our "main". */
  def program[F[_]: Monad: LiftIO: AppConfig]: F[Unit] = for {
    env     <- ApplicativeAsk.ask[F, Env]
    result  <- env.ping(env.websiteToPing).to[F]
    _       <- env.print(s"${env.websiteToPing}: $result").to[F]
  } yield ()

  /** Type alias representing an effectively ReaderT effect. */
  type AppConfig[F[_]] = ApplicativeAsk[F, Env]

  /** The environment our program will use. */
  final case class Env(
    print: String => IO[Unit],
    ping: Url => IO[Int],
    websiteToPing: Url
  )

  /** Lift {{requests.get}} into the IO effect. */
  def pingWebsite(url: Url): IO[Int] =
    IO(requests.get(url.value).statusCode)

  /** Lift {{println}} into the IO effect. */
  def ioPrint(s: String): IO[Unit] =
    IO.pure(s).flatMap(toPrint => {
      println(toPrint)
      IO.unit
    })
}