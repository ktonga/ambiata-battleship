package ambiata

import scalaz.syntax.std.option._
import scalaz.Enum
import scalaz.syntax.enum._

package object battleship {
  def succnx[F: Enum](n: Int, a: F): Option[F] = (0 until n).foldLeft(a.some) { (_a, _) => _a.flatMap(_.succx) }
  def prednx[F: Enum](n: Int, a: F): Option[F] = (0 until n).foldLeft(a.some) { (_a, _) => _a.flatMap(_.predx) }
}
