package ambiata.battleship

import scalaz.syntax.id._
import scalaz.syntax.enum._
import scalaz.syntax.std.option._
import scalaz.std.option._
import scalaz.std.anyVal._
import scalaz.{ Equal, Enum, Ordering, Lens }
import Grid._


trait Order { val order: Int }
abstract class EnumFromSeq[F <: Order](all: Seq[F]) extends Enum[F] {
  def pred(f: F): F = if(f == min.get) max.get else all(f.order - 1)
  def succ(f: F): F = if(f == max.get) min.get else all(f.order + 1)
  def order(f1: F, f2: F): Ordering = f1.order ?|? f2.order
  override def min: Option[F] = all.head.some
  override def max: Option[F] = all.last.some
}

sealed trait Row extends Order
object Row {
  implicit object RowEnum extends EnumFromSeq(Rows.all)
}
object Rows {
  case object `01` extends Row { val order = 0 }
  case object `02` extends Row { val order = 1 }
  case object `03` extends Row { val order = 2 }
  case object `04` extends Row { val order = 3 }
  case object `05` extends Row { val order = 4 }
  case object `06` extends Row { val order = 5 }
  case object `07` extends Row { val order = 6 }
  case object `08` extends Row { val order = 7 }
  case object `09` extends Row { val order = 8 }
  case object `10` extends Row { val order = 9 }
  val all: Seq[Row] = Seq(`01`, `02`, `03`, `04`, `05`, `06`, `07`, `08`, `09`, `10`) 
}

sealed trait Col extends Order
object Col {
  implicit object ColEnum extends EnumFromSeq(Cols.all)
}
object Cols {
  case object A extends Col { val order = 0 }
  case object B extends Col { val order = 1 }
  case object C extends Col { val order = 2 }
  case object D extends Col { val order = 3 }
  case object E extends Col { val order = 4 }
  case object F extends Col { val order = 5 }
  case object G extends Col { val order = 6 }
  case object H extends Col { val order = 7 }
  case object I extends Col { val order = 8 }
  case object J extends Col { val order = 9 }
  val all: Seq[Col] = Seq(A, B, C, D, E, F, G, H, I, J) 
}

case class Square[A](row: Row, col: Col, value: Option[A]) {
  def withValue(a: A) = this.copy(value = a.some)
}

object Square {

  type Predicate = Square[_] => Boolean

  def predicateSingle(row: Row, col: Col): Predicate =
    { sqr => sqr.row === row && sqr.col === col }

  def predicateHRange(row: Row, colFrom: Col, colTo: Col): Predicate =
    { sqr => sqr.row === row && sqr.col >= colFrom && sqr.col <= colTo }

  def predicateVRange(rowFrom: Row, rowTo: Row, col: Col): Predicate =
    { sqr => sqr.col === col && sqr.row >= rowFrom && sqr.row <= rowTo }

}

case class Grid[A] private (sqrs: Squares[A])

object Grid {

  type Squares[A] = Seq[Square[A]]

  def sqrsL[A]: Lens[Grid[A], Squares[A]] = Lens.lensu( { (g, ss) => g.copy(sqrs = ss) }, { _.sqrs } )

  def empty[A] = (for {
    r <- Rows.all
    c <- Cols.all
  } yield Square(r, c, none[A])) |> { sqrs => Grid(sqrs) }

  def update[A](p: Square.Predicate, a: A)(grid: Grid[A]): Grid[A] =
    sqrsL.mod({ _.map { sqr => if(p(sqr)) sqr.withValue(a) else sqr } }, grid)

}



