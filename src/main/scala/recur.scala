package recur

import cats._
import cats.data._
import cats.implicits._

object Morphism {
  case class Fix[F[_]](unFix: F[Fix[F]])

  def cata[F[_]: Functor, A](outer: Fix[F])(f: F[A] => A): A =
    f(outer.unFix.map { inner =>
      cata(inner)(f)
    })

  // dual of cata
  def ana[F[_]: Functor, A](init: A)(f: A => F[A]): Fix[F] =
    Fix(f(init).map { a =>
      ana(a)(f)
    })

  // cata*
  def para[F[_]: Functor, A](outer: Fix[F])(f: F[(A, Fix[F])] => A): A =
    f(outer.unFix.map { inner =>
      (para(inner)(f), inner)
    })

  // ana*
  def apo[F[_]: Functor, A](init: A)(f: A => F[Either[A, Fix[F]]]): Fix[F] =
    Fix(f(init).map {
      case Left(a) => apo(a)(f)
      case Right(fix) => fix
    })

  def hylo[F[_]: Functor, A, B](init: A)(f: A => F[A], g: F[B] => B): B =
    g(f(init).map { a =>
      hylo(a)(f, g)
    })
}

import Morphism._

object Demo {

  // AST
  sealed abstract class ExprF[A]
  case class Value[A](x: Long) extends ExprF[A]
  case class Multiply[A](lhs: A, rhs: A) extends ExprF[A]

  object ExprF {
    implicit val functor: Functor[ExprF] =
      new Functor[ExprF] {
        def map[A, B](e: ExprF[A])(f: A => B): ExprF[B] =
          e match {
            case Value(x) => Value(x)
            case Multiply(x, y) => Multiply(f(x), f(y))
          }
      }
  }

  type Expr = Fix[ExprF]

  def value(x: Long): Expr = Fix(Value(x))
  def multiply(e1: Expr, e2: Expr): Expr = Fix(Multiply(e1, e2))

  def evaluate(fix: Expr): Long =
    cata(fix)((e: ExprF[Long]) =>
      e match {
        case Value(x) => x
        case Multiply(x: Long, y: Long) => x * y
      })

  def findFactor(n: Long): Option[Long] =
    if (n <= 2) None
    else if (n % 2 == 0) Some(2L)
    else (3L to math.sqrt(n).toLong by 2L).find(n % _ == 0)

  def factor(x: Long): Expr =
    ana(x)((n: Long) =>
      findFactor(n) match {
        case None => Value(n)
        case Some(divisor) => Multiply(divisor, n / divisor)
      })

  def debug(fix: Expr): Long =
    para[ExprF, Long](fix) {
      case Value(x) =>
        x
      case Multiply((x, atree), (y, btree)) =>
        val res = x * y
        val m = multiply(atree, btree) // tree
        println(s"debug: got $res from $m")
        res
    }

  def factorial(n: Long): Expr =
    apo[ExprF, Long](n) { x =>
      if (x < 1) Value(0L)
      else if (x == 1) Value(1L)
      else Multiply(Right(value(x)), Left(x - 1))
    }

  def main(args: Array[String]): Unit = {
    val expr = multiply(value(3), multiply(value(4), value(5)))
    val x = evaluate(expr)
    println(s"evaluate($expr) -> $x")

    val primes = factor(x)
    println(s"factor($x) -> $primes")

    val y = debug(multiply(multiply(value(2), value(3)), multiply(value(4), value(5))))
    println(s"debug($expr) -> $y")

    val prod = factorial(9L) // 9 * 8 * 7 * ... * 1
    println(s"factorial(9) -> $prod")
  }
}
