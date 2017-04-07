package recur

import cats._
import cats.data._
import cats.implicits._

object Problems {
  case class Fix[F[_]](unFix: F[Fix[F]])

  def cata[F[_]: Functor, A](outer: Fix[F])(f: F[A] => A): A = ???

  def para[F[_]: Functor, A](outer: Fix[F])(f: F[(A, Fix[F])] => A): A = ???

  def ana[F[_]: Functor, A](init: A)(f: A => F[A]): Fix[F] = ???

  def apo[F[_]: Functor, A](init: A)(f: A => F[Either[A, Fix[F]]]): Fix[F] = ???

  def hylo[F[_]: Functor, A, B](init: A)(f: A => F[A], g: F[B] => B): B = ???
}
