package recur

import cats._
import cats.implicits._
import java.io.File

import Morphism._

/**
 * This is a port of this Haskell code to Scala using Cats.
 *
 * https://www.reddit.com/r/haskell/comments/cs54i/how_would_you_write_du_in_haskell/c0uvqqo
 *
 * Most of this file is just setting up things we don't currently
 * have:
 *
 * 1. Basic I/O type (Io)
 * 2. Open-recursive filesystem type (Fs) with java.io-based methods
 * 3. Type class instances for Fs (Monad, Traverse, etc.)
 * 4. The actual "du" code.
 */

/**
 * Set up a couple nice type aliases
 */
object Types {
  type Path = String
  type Size = Long
}

import Types._

/**
 * Open-recursive FS type.
 */
sealed abstract class Fs[R] {
  def fold[A](f: Path => A, g: (Path, Stream[R]) => A): A =
    this match {
      case Fs.IsFile(p) => f(p)
      case Fs.IsDir(p, rs) => g(p, rs)
    }
}

object Fs {

  case class IsFile[R](p: Path) extends Fs[R]
  case class IsDir[R](p: Path, rs: Stream[R]) extends Fs[R]

  def file[R](p: Path): Fs[R] = IsFile(p)
  def dir[R](p: Path, rs: Stream[R]): Fs[R] = IsDir(p, rs)

  def filesize(p: Path): Io[Size] =
    Io(new File(p).length)

  def isdir(p: Path): Io[Boolean] =
    Io(new File(p).isDirectory)

  def ls(p: Path): Io[Stream[Path]] =
    Io(Stream(new File(p).listFiles: _*).map(_.getPath))

  implicit val fsTraverse: Traverse[Fs] =
    new Traverse[Fs] {
      override def map[A, B](fa: Fs[A])(f: A => B): Fs[B] =
        fa.fold(p => Fs.file(p), (p, as) => Fs.dir(p, as.map(f)))
      def traverse[G[_]: Applicative, A, B](fa: Fs[A])(f: A => G[B]): G[Fs[B]] =
        fa.fold(Fs.file(_).pure[G], (p, as) => as.traverse(f).map(Fs.dir(p, _)))
      def foldLeft[A, B](fa: Fs[A], b: B)(f: (B, A) => B): B =
        fa.fold(_ => b, (_, as) => as.foldLeft(b)(f))
      def foldRight[A, B](fa: Fs[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.fold(_ => lb, (_, as) => as.foldRight(lb)(f))
    }
}

// presentation version, using hylo

object DuNew {

  val f: Path => Fs[Path] = { p =>
    if (Fs.isdir(p).unsafeRun) Fs.dir(p, Fs.ls(p).unsafeRun)
    else Fs.file(p)
  }
  def getFiles(p: Path): Fix[Fs] =
    ana(p)(f)

  val g: Fs[Size] => Size = {
    case Fs.IsFile(path) => Fs.filesize(path).unsafeRun
    case Fs.IsDir(_, sizes) => sizes.sum
  }
  def sumFiles(outer: Fix[Fs]): Size =
    cata[Fs, Size](outer)(g)

  def du(p: Path): Size =
    hylo[Fs, Path, Size](p)(f, g)

  def main(args: Array[String]): Unit = {
    val path = if (args.isEmpty) "." else args(0)
    println(du(path))
  }
}

// previous version, implemented in terms of a monad (in this case Io).

object DuOrig {

  def hyloM[F[_]: Traverse, M[_]: Monad, A, B](g: A => M[F[A]], f: F[B] => M[B]): A => M[B] =
    (a: A) => g(a).flatMap(_.traverse(hyloM(g, f)).flatMap(f))

  def getFiles(p: Path): Io[Fs[Path]] =
    Fs.isdir(p).flatMap { b =>
      if (b) Fs.ls(p).map(Fs.dir(p, _)) else Io(Fs.file(p))
    }

  def sumFiles(fs: Fs[Size]): Io[Size] =
    fs.fold(
      p => Fs.filesize(p),
      (p, ns) => Fs.filesize(p).map(_ + ns.foldLeft(0L)(_ + _)))

  def du(p: Path): Io[Size] =
    hyloM(getFiles, sumFiles).apply(p)

  def main(args: Array[String]): Unit = {
    val path = if (args.isEmpty) ".." else args(0)
    println(s"getting total size of: $path")
    du(path).map(println(_)).unsafeRun
  }
}

