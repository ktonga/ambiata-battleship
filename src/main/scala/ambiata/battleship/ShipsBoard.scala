package ambiata.battleship


import scalaz.\/
import scalaz.syntax.std.option._
import scalaz.{ Enum, Lens }
import scalaz.syntax.enum._
import scalaz.syntax.id._
import scala.util.Random
import ShipsBoard._

sealed trait Ship { val size: Int }
object Ships {
  case object Carrier    extends Ship { val size = 5 }
  case object Battleship extends Ship { val size = 4 }
  case object Submarine  extends Ship { val size = 3 }
  case object Cruiser    extends Ship { val size = 2 }
  case object Patrol     extends Ship { val size = 1 }
  val all: Seq[Ship] = Seq(Carrier, Battleship, Submarine, Cruiser, Patrol)
}

sealed trait Orientation
object Orientations {
  case object Horizontal extends Orientation
  case object Vertical   extends Orientation
}

case class ShipsBoard private(grid: Grid[Ship]) {

  def print(): Unit = {
    val colsLine = Cols.all.map { col => s" ${col.toString} " } mkString(s"   |", "|", "|") 
    println(colsLine)
    grid.sqrs.grouped(10).zipWithIndex.foreach {
      case (row, idx) =>
        val line = row.map { sqr => s" ${sqr.value.fold(" "){_ => "X"}} " } mkString(s" $idx |", "|", "|") 
        println(Seq.fill(line.length)('-').mkString)
        println(line)
    }
    println(Seq.fill(colsLine.length)('-').mkString)
  }

}

object ShipsBoard {

  val gridL: Lens[ShipsBoard, Grid[Ship]] = Lens.lensu( { (sb, g) => sb.copy(grid = g) }, { _.grid } )

  def empty = ShipsBoard(Grid.empty[Ship])

  def addShip(ship: Ship, row: Row, col: Col, orientation: Orientation): ShipsBoard => \/[String, ShipsBoard] =
    { board =>
      for {
        _ <- shipPresent(board, ship)
        p <- buildPredicate(ship, row, col, orientation)
        _ <- shipOverlaps(board, p)
      } yield gridL.mod(Grid.update(p, ship), board)
    }

  val addShipIgnoreError = Function.untupled( (addShip _).tupled.andThen { f => { b: ShipsBoard => f(b).fold(_ => b, identity) } } )

  private def shipPresent(board: ShipsBoard, ship: Ship): \/[String, Unit] =
    if(board.grid.sqrs.exists { sqr => sqr.value.exists(_ == ship) })
      s"Ship $ship already present in board".left[Unit]
    else ().right[String]

  private def buildPredicate(ship: Ship, row: Row, col: Col, orientation: Orientation): \/[String, Square.Predicate] = {
    (orientation match {
      case Orientations.Horizontal =>
        succnx(ship.size - 1, col) map { toCol => Square.predicateHRange(row, col, toCol) }
      case Orientations.Vertical   =>
        succnx(ship.size - 1, row) map { toRow => Square.predicateVRange(row, toRow, col) }
    }) \/> s"Ship $ship with size ${ship.size} doesn't fit in give position ($row, $col, $orientation)"
  }

  private def shipOverlaps(board: ShipsBoard, p: Square.Predicate): \/[String, Unit] =
    if(board.grid.sqrs.exists { sqr => p(sqr) && sqr.value.isDefined })
      "Ship overlaps with another ship".left[Unit]
    else ().right[String]

  def random = Ships.all.foldLeft(empty) {
    (board, ship) => tryAddShip(board, ship)
  }

  private def tryAddShip(tryBoard: ShipsBoard, tryShip: Ship): ShipsBoard = {
    val row = Rows.all(Random.nextInt(Rows.all.size - 1))
    val col = Cols.all(Random.nextInt(Cols.all.size - 1))
    val orient = if(Random.nextBoolean) Orientations.Horizontal else Orientations.Vertical
    addShip(tryShip, row, col, orient)(tryBoard).fold(_ => tryAddShip(tryBoard, tryShip), identity)
  }
}
