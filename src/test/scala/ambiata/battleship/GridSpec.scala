package ambiata.battleship

import org.specs2.{ Specification, ScalaCheck }
import org.scalacheck.Prop.forAll
import scalaz.scalacheck.ScalazProperties._
import scalaz.syntax.std.option._
import scalaz.syntax.enum._


class GridSpec extends Specification with ScalaCheck {

  import Arbitraries._

  def is = s2"""
    Row must obey Enum laws                             ${enum.laws[Row]}
    Col must obey Enum laws                             ${enum.laws[Col]}
    An Empty Grid must have the right amount of squares $emptyGridSize
    An Empty Grid must have value none for all squares  $emptyGridValues
    A Grid Square can be updated                        $updateSquare
    A Grid horizontal Square range can be updated       $updateHSquareRange
    A Grid vertical Square range can be updated         $updateVSquareRange
  """

  def emptyGridSize   = Grid.empty[Int].sqrs must have size(Rows.all.size * Cols.all.size)
  def emptyGridValues = Grid.empty[Int].sqrs must contain(beNone ^^ { (s: Square[Int]) => s.value }).forall

  def updateSquare = forAll { (row: Row, col: Col, a: Int) =>
    val updated = Grid.update(Square.predicateSingle(row, col), a)(Grid.empty[Int])
    updated.sqrs must (contain(beSome(a) ^^ { (s: Square[Int]) => s.value }).exactly(1)
                 and   contain(===(Square(row, col, a.some))))
  }

  def updateHSquareRange = forAll { (row: Row, colFrom: Col, colTo: Col, a: Int) => (colTo > colFrom) ==> {
    val updated = Grid.update(Square.predicateHRange(row, colFrom, colTo), a)(Grid.empty[Int])
    updated.sqrs must (contain(beSome(a) ^^ { (s: Square[Int]) => s.value }).exactly(colTo.order - colFrom.order + 1)
      and   contain(allOf(colFrom |-> colTo map { col => Square(row, col, a.some) }:_*)))
  }}

  def updateVSquareRange = forAll { (rowFrom: Row, rowTo: Row, col: Col, a: Int) => (rowTo > rowFrom) ==> {
    val updated = Grid.update(Square.predicateVRange(rowFrom, rowTo, col), a)(Grid.empty[Int])
    updated.sqrs must (contain(beSome(a) ^^ { (s: Square[Int]) => s.value }).exactly(rowTo.order - rowFrom.order + 1)
      and   contain(allOf(rowFrom |-> rowTo map { row => Square(row, col, a.some) }:_*)))
  }}

}
