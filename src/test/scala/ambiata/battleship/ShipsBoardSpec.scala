package ambiata.battleship

import org.specs2.{ Specification, ScalaCheck }
import org.scalacheck.Prop.forAll
import scalaz.scalacheck.ScalazProperties._
import scalaz.syntax.std.option._
import scalaz.syntax.enum._
import scalaz.syntax.id._
import scalaz.\/-


class ShipsBoardSpec extends Specification with ScalaCheck {

  import ShipsBoard._
  import Arbitraries._

  def is = s2"""
    A ship can be added to the board                        $addAShip
    A ship can not be added twise                           $addAShipPresent
    A ship must fit in board                                $addAShipFit
    A ship must not overlap                                 $addAShipOverlap
    A random board can be created with all ships            $randomBoard
  """

  def addAShip = forAll { (valid: Valid[AddShipParams]) =>
    val params = valid.params
    val board = addShip(params.ship, params.row, params.col, params.orientation)(ShipsBoard.empty)
    board.map(_.grid.sqrs).toEither must beRight(contain(beSome(params.ship) ^^ { (s: Square[Ship]) => s.value }).exactly(params.ship.size))
  }

  def addAShipPresent = forAll { (valid: Valid[AddShipParams]) =>
    val params = valid.params
    val board = addShip(params.ship, params.row, params.col, params.orientation)(ShipsBoard.empty).getOrElse(throw new Exception)
    val secondAddResult = (valid match {
      case Valid(p @ AddShipParams(ship, row, col, Orientations.Horizontal)) => p.copy(row = row -+- 1)
      case Valid(p @ AddShipParams(ship, row, col, Orientations.Vertical))   => p.copy(col = col -+- 1)
    }) |> { secondAdd => addShip(secondAdd.ship, secondAdd.row, secondAdd.col, secondAdd.orientation)(board) }
    secondAddResult.toEither must beLeft(s"Ship ${params.ship} already present in board")
  }

  def addAShipFit = forAll { (invalid: Invalid[AddShipParams]) =>
    val params = invalid.params
    val addResult = addShip(params.ship, params.row, params.col, params.orientation)(ShipsBoard.empty)
    addResult.toEither must beLeft(s"Ship ${params.ship} with size ${params.ship.size} doesn't fit in give position (${params.row}, ${params.col}, ${params.orientation})")
  }

  def addAShipOverlap = forAll { (overlap: Overlap) =>
    val Overlap(params1, params2) = overlap
    val board = addShip(params1.ship, params1.row, params1.col, params1.orientation)(ShipsBoard.empty).getOrElse(throw new Exception)
    val secondAddResult = addShip(params2.ship, params2.row, params2.col, params2.orientation)(board)
    secondAddResult.toEither must beLeft("Ship overlaps with another ship")
  }

  def randomBoard = {
    val sizeByShip = ShipsBoard.random.grid.sqrs.groupBy(_.value).collect { case (Some(s), l) => (s, l.size) }
    sizeByShip must contain(allOf(Ships.all.map {s => (s, s.size) }:_*))
  }

}
