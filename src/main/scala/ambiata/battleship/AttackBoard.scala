package ambiata.battleship


import scalaz.{ \/, Lens }
import scalaz.syntax.id._
import AttackBoard._

sealed trait AttackResult
object AttackResults {
  case object Hit  extends AttackResult
  case object Miss extends AttackResult
}

case class AttackBoard private(grid: Grid[AttackResult]) {

  def print(): Unit = {
    val colsLine = Cols.all.map { col => s" ${col.toString} " } mkString(s"   |", "|", "|") 
    println(colsLine)
    grid.sqrs.grouped(10).zipWithIndex.foreach {
      case (row, idx) =>
        val line = row.map { sqr =>
          s" ${sqr.value.fold(" "){r => if(r == AttackResults.Hit) "H" else "M"}} "
        } mkString(s" $idx |", "|", "|") 
        println(Seq.fill(line.length)('-').mkString)
        println(line)
    }
    println(Seq.fill(colsLine.length)('-').mkString)
  }

}

object AttackBoard {

  val gridL: Lens[AttackBoard, Grid[AttackResult]] = Lens.lensu( { (ab, g) => ab.copy(grid = g) }, { _.grid } )

  def empty = AttackBoard(Grid.empty[AttackResult])

  def attack(row: Row, col: Col, shipsBoard: ShipsBoard): AttackBoard => \/[String, (AttackResult, AttackBoard)] =
  { board =>
    Square.predicateSingle(row, col) |> { p =>
      board.grid.sqrs.find(p).flatMap(_.value).fold {
        shipsBoard.grid.sqrs.find(p).flatMap(_.value).fold[AttackResult](AttackResults.Miss)(_ => AttackResults.Hit) |> { result =>
          (result, gridL.mod(Grid.update(p, result), board)).right[String]
        }
      } { _ =>
        s"Attack to ($row, $col) already done before".left[(AttackResult, AttackBoard)]
      }
    }
  }
  
  val attackIgnoreError = Function.untupled( (attack _).tupled.andThen { f => { b: AttackBoard => f(b).fold(_ => (AttackResults.Miss, b), identity) } } )

  val hitsToWin: Int = Ships.all.map(_.size).sum

  def winner(attackBoard: AttackBoard): Boolean = attackBoard.grid.sqrs.count(_.value.exists(_ == AttackResults.Hit)) == hitsToWin

}
