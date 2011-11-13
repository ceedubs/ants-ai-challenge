sealed trait Positionable {
  val tile: Tile
}

sealed trait Ant extends Positionable

case class MyAnt(tile: Tile) extends Ant
case class EnemyAnt(tile: Tile) extends Ant
case class Corpse(tile: Tile) extends Positionable
case class Food(tile: Tile) extends Positionable
case class Water(tile: Tile) extends Positionable
case class MyHill(tile: Tile) extends Positionable
case class EnemyHill(tile: Tile) extends Positionable
