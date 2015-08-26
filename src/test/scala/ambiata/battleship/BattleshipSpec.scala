package ambiata.battleship

import org.specs2.{ Specification, ScalaCheck }
import org.scalacheck._
import scalaz.scalacheck.ScalazProperties._
import scalaz._
import Scalaz._


class BattleshipSpec extends Specification with ScalaCheck {

  import Arbitraries._
  import Prop._

  def is = s2"""
    Battleship should let you start a new game with empty boards                       $startEmptyGame
    Battleship should let you start a new game with random boards                      $startRandomGame
    Setup should be finished after placing all the ships for both players              $finishSetup
    Game state shold be updated after an attack                                        $stateAfterAttack
    Game should finish after a player hits all the ships of the other player           $finishGame
  """

  def startEmptyGame = {
    val game = Battleship.emptyGame("Player One", "Player Two")
    (game.players._1.name === "Player One") and
    (game.unplacedShips._1 must contain(exactly(Ships.all:_*))) and
    (game.players._2.name === "Player Two") and
    (game.unplacedShips._2 must contain(exactly(Ships.all:_*)))
  }

  def startRandomGame = {
    val game = Battleship.randomGame("Player One", "Player Two")
    (game.players._1.name === "Player One") and
    (game.unplacedShips._1 must be empty) and
    (game.players._2.name === "Player Two") and
    (game.unplacedShips._2 must be empty)
  }

  def finishSetup = {
    val playersShips: List[(PlayerRef, Ship, Row)] = for {
      pRef <- List(Players.P1, Players.P2)
      (ship, row) <- Ships.all.zip(Rows.all)
    } yield (pRef, ship, row)
    val results = playersShips.traverseS {
      case (p, s, r) => Battleship.addShip(p, s, r, Cols.A, Orientations.Horizontal)
    }.eval(Battleship.emptyGame("Player One", "Player Two"))
    results must containTheSameElementsAs(List.fill(results.size - 1)(false) :+ true)
  }

  def stateAfterAttack = forAll { (row: Row, col: Col) =>
    val (game, \/-(result)) = Battleship.attack(row, col).run(Battleship.randomGame("Player One", "Player Two"))
    val attackValue = game.players._1.attackBoard.grid.sqrs.find(Square.predicateSingle(row, col)).flatMap(_.value)
    (attackValue must beSome(result)) and (game.turn === (Players.P2, Players.P1))
  }

  def finishGame = {
    val game = Battleship.randomGame("Loser", "Winner")
    val loserAttacks = for {
      row  <- Rows.all
      col <- Cols.all
    } yield (row, col)
    // let's make P2 the winner
    val winnerAttacks = game.players._1.shipsBoard.grid.sqrs.filter(_.value.isDefined).map(x => (x.row, x.col))

    val attacks = loserAttacks.zip(winnerAttacks).flatMap(x => Seq(x._1, x._2))

    val finishedGame: Game = attacks.toList.traverseS {
      case (r, c) => Battleship.attack(r, c)
    }.exec(game)

    finishedGame.winner must beSome(Players.P2)
  }

}
