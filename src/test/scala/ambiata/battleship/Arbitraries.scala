package ambiata.battleship

import org.scalacheck.{ Arbitrary, Gen }
import Arbitrary.arbitrary
import scalaz.Enum
import scalaz.syntax.enum._


object Arbitraries {

  implicit val RowArbitrary: Arbitrary[Row] = Arbitrary(Gen.oneOf(Rows.all))

  implicit val ColArbitrary: Arbitrary[Col] = Arbitrary(Gen.oneOf(Cols.all))

  implicit val ShipArbitrary: Arbitrary[Ship] = Arbitrary(Gen.oneOf(Ships.all))

  implicit val OrientationArbitrary: Arbitrary[Orientation] =
    Arbitrary(Gen.oneOf(Seq(Orientations.Horizontal, Orientations.Vertical)))

  case class AddShipParams(ship: Ship, row: Row, col: Col, orientation: Orientation)
  case class Valid[P](params: P)
  case class Invalid[P](params: P)
  case class Overlap(params1: AddShipParams, params2: AddShipParams)

  def enumUntil[F: Enum](n: Int): Gen[F] =
    Gen.oneOf(Enum[F].min.get |-> Enum[F].max.get --- n)

  implicit val ValidAddShipArbitrary: Arbitrary[Valid[AddShipParams]] =
    Arbitrary {
      for {
        ship   <- arbitrary[Ship]
        orient <- arbitrary[Orientation]
        row    <- if(orient == Orientations.Horizontal) arbitrary[Row] else enumUntil[Row](ship.size - 1)
        col    <- if(orient == Orientations.Vertical) arbitrary[Col] else enumUntil[Col](ship.size - 1)
      } yield Valid(AddShipParams(ship, row, col, orient))
    }

  def enumFrom[F: Enum](n: Int): Gen[F] =
    Gen.oneOf(Enum[F].max.get --- n |-> Enum[F].max.get)

  implicit val InvalidAddShipArbitrary: Arbitrary[Invalid[AddShipParams]] =
    Arbitrary {
      for {
        ship   <- Gen.oneOf(Ships.all.init)
        orient <- arbitrary[Orientation]
        row    <- if(orient == Orientations.Horizontal) arbitrary[Row] else enumFrom[Row](ship.size - 2)
        col    <- if(orient == Orientations.Vertical) arbitrary[Col] else enumFrom[Col](ship.size - 2)
      } yield Invalid(AddShipParams(ship, row, col, orient))
    }

  def enumBetweenParallel[F](a: F, s1: Int, s2: Int)(implicit E: Enum[F]): Gen[F] = {
    val start: F = prednx[F](s2 - 1, a).getOrElse(E.min.get)
    val end: F = E.min(a -+- (s1 - 1), E.max.get --- (s2 - 1))
    Gen.oneOf(start |-> end)
  }

  def enumBetween[F](a: F, n: Int)(implicit E: Enum[F]): Gen[F] = Gen.oneOf(a |-> a -+- (n - 1))

  def enumBetweenDivergent[F](a: F, n: Int)(implicit E: Enum[F]): Gen[F] = {
    val start: F = prednx[F](n - 1, a).getOrElse(E.min.get)
    val end: F = E.min(a, E.max.get --- (n - 1))
    Gen.oneOf(start |-> end)
  }

  def overlapingRowCol(p1: AddShipParams, ship: Ship, orient: Orientation): Gen[(Row, Col)] =
    (p1.orientation, orient) match {
      case (Orientations.Horizontal, Orientations.Horizontal) =>
        enumBetweenParallel[Col](p1.col, p1.ship.size, ship.size).map((p1.row, _))
      case (Orientations.Vertical, Orientations.Vertical) =>
        enumBetweenParallel[Row](p1.row, p1.ship.size, ship.size).map((_, p1.col))
      case (Orientations.Horizontal, Orientations.Vertical) =>
        for {
          row <- enumBetweenDivergent(p1.row, ship.size)
          col <- enumBetween[Col](p1.col, p1.ship.size)
        } yield (row, col)
      case (Orientations.Vertical, Orientations.Horizontal) =>
        for {
          row <- enumBetween[Row](p1.row, p1.ship.size)
          col <- enumBetweenDivergent(p1.col, ship.size)
        } yield (row, col)
    }

  implicit val OverlapAddShipArbitrary: Arbitrary[Overlap] =
    Arbitrary {
      for {
        Valid(params1) <- arbitrary[Valid[AddShipParams]]
        ship           <- Gen.oneOf(Ships.all diff Seq(params1.ship))
        orient         <- arbitrary[Orientation]
        (row, col)     <- overlapingRowCol(params1, ship, orient)
      } yield Overlap(params1, AddShipParams(ship, row, col, orient))
    }

}
