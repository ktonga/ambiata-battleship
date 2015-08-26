package ambiata.battleship

import scalaz.{ Lens, State, \/ }
import scalaz.std.option._
import scalaz.syntax.state._
import scalaz.syntax.id._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import Battleship.Winner

sealed trait PlayerRef
object Players {
  case object P1 extends PlayerRef
  case object P2 extends PlayerRef
}

case class Player(name: String, shipsBoard: ShipsBoard, attackBoard: AttackBoard)

case class Game(players: (Player, Player), unplacedShips: (Seq[Ship], Seq[Ship]), turn: (PlayerRef, PlayerRef), winner: Winner)

object Lenses {
  type PlayerLens = Lens[Game, Player]
  type UnplacedShipsLens = Lens[Game, Seq[Ship]]

  val shipsBoardL: Lens[Player, ShipsBoard] = Lens.lensu( (p, sb) => p.copy(shipsBoard = sb), _.shipsBoard )
  val attackBoardL: Lens[Player, AttackBoard] = Lens.lensu( (p, ab) => p.copy(attackBoard = ab), _.attackBoard )
  val playersL: Lens[Game, (Player, Player)] = Lens.lensu( (g, ps) => g.copy(players = ps), _.players )
  val turnL: Lens[Game, (PlayerRef, PlayerRef)] = Lens.lensu( (g, t) => g.copy(turn = t), _.turn )
  val winnerL: Lens[Game, Winner] = Lens.lensu( (g, w) => g.copy(winner = w), _.winner )
  val unplacedShipsL: Lens[Game, (Seq[Ship], Seq[Ship])] = Lens.lensu( (g, uss) => g.copy(unplacedShips = uss), _.unplacedShips )

  def playerL(ref: PlayerRef): PlayerLens = playersL >=> (ref match {
    case Players.P1 => Lens.firstLens
    case Players.P2 => Lens.secondLens
  })

  def unplacedShipsL(ref: PlayerRef): UnplacedShipsLens = unplacedShipsL >=> (ref match {
    case Players.P1 => Lens.firstLens
    case Players.P2 => Lens.secondLens
  })

  def turnPlayerL: Lens[Game, PlayerRef] = turnL >=> Lens.firstLens
  def turnOtherL:  Lens[Game, PlayerRef] = turnL >=> Lens.secondLens

}

object Battleship {
  import State._
  import Lenses._
  import ShipsBoard._
  import AttackBoard._
  
  type GameState[A] = State[Game, A]
  type Winner = Option[PlayerRef]

  val emptyGame = newGame(ShipsBoard.empty, Ships.all) _
  val randomGame = newGame(ShipsBoard.random, Seq()) _
    
  private def newGame(shipsBoard: ShipsBoard, unplacedShips: Seq[Ship])(n1: String, n2: String): Game =
    Game(
      (Player(n1, shipsBoard, AttackBoard.empty), Player(n2, shipsBoard, AttackBoard.empty)),
      (unplacedShips, unplacedShips), (Players.P1, Players.P2), None
    )

  /**
   * Place the given ship on the player's ships board.
   * Returns true if all the ships were already placed so the game setup is finished.
   */
  def addShip(pRef: PlayerRef, ship: Ship, row: Row, col: Col, orientation: Orientation): GameState[Boolean] =
    for {
      _  <- playerL(pRef) >=> shipsBoardL %== addShipIgnoreError(ship, row, col, orientation)
      _  <- unplacedShipsL(pRef) %== { _ diff Seq(ship) }
      finished <- setupFinished
    } yield finished

  private def setupFinished: GameState[Boolean] =
    for {
      rss1 <- unplacedShipsL(Players.P1)
      rss2 <- unplacedShipsL(Players.P2)
    } yield rss1.isEmpty && rss2.isEmpty

  /**
   * Performs the attack for the turn-holder player.
   * Returns either the attack result, hit or miss, or the winner player ref if the attack completed the game.
   */
  def attack(row: Row, col: Col): GameState[PlayerRef \/ AttackResult] =
    for {
      pRef <- turnPlayerL
      oRef <- turnOtherL
      abL = playerL(pRef) >=> attackBoardL
      ab <- abL
      oShipsBoard  <- playerL(oRef) >=> shipsBoardL
      ar = attackIgnoreError(row, col, oShipsBoard)(ab)
      (result, _ab) = ar
      _ <- abL := _ab
      _ <- turnL %== { _.swap }
      winner <- winnerL := (if(AttackBoard.winner(_ab)) pRef.some else none[PlayerRef])
    } yield winner <\/ result
  

}
