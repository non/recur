package recur

import cats._
import cats.implicits._

/**
 * Basic I/O type.
 */
class Io[A] private[recur] (private[recur] val e: Eval[A]) {
  def map[B](f: A => B): Io[B] = new Io(e.map(f))
  def flatMap[B](f: A => Io[B]): Io[B] = new Io(e.flatMap(a => f(a).e))

  def unsafeRun: A = e.value
}

object Io {
  def apply[A](a: => A): Io[A] = new Io(Always(a))

  implicit val ioMonad: Monad[Io] =
    new Monad[Io] {
      def pure[A](a: A): Io[A] = new Io(Now(a))
      override def map[A, B](fa: Io[A])(f: A => B): Io[B] = fa.map(f)
      def flatMap[A, B](fa: Io[A])(f: A => Io[B]): Io[B] = fa.flatMap(f)
      def tailRecM[A, B](a: A)(f: A => Io[Either[A, B]]): Io[B] =
        new Io(f(a).e.flatMap {
          case Left(a) => tailRecM(a)(f).e
          case Right(b) => Now(b)
        })
    }
}
