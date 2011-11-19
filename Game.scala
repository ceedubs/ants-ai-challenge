import scala.math.{abs,min,pow}

case class GameInProgress(turn: Int = 0, turnStartTime: Long = System.currentTimeMillis(), parameters: GameParameters = GameParameters(), board: Board = Board()) extends Game {
  val gameOver = false
  def including[P <: Positionable](positionable: P) = this.copy(board = this.board including positionable)
  def including(p: Positionable*): GameInProgress = p.foldLeft(this){(game, positionable) => game.including(positionable)}
}
case class GameOver(turn: Int = 0, turnStartTime: Long = System.currentTimeMillis(), parameters: GameParameters = GameParameters(), board: Board = Board()) extends Game {
  val gameOver = true
}

sealed trait Game {
  val turn: Int
  val turnStartTime: Long
  val parameters: GameParameters
  val board: Board
  val gameOver: Boolean

  def distanceFrom(one: Tile) = new {
    def to(another: Tile) = {
      val dRow = abs(one.row - another.row)
      val dCol = abs(one.column - another.column)
      pow(min(dRow, parameters.rows - dRow), 2) + pow(min(dCol, parameters.columns - dCol), 2)
    }
  }

  def directionFrom(one: Tile) = new {
    def to(other: Tile): Set[CardinalPoint] = {
      val ns: Set[CardinalPoint] = if (one.row < other.row) {
        if (other.row - one.row >= parameters.rows / 2) Set(North) else Set(South)
      } else if (one.row > other.row) {
        if (one.row - other.row >= parameters.rows / 2) Set(South) else Set(North)
      } else Set()

      val ew: Set[CardinalPoint] = if (one.column < other.column) {
        if (other.column - one.column >= parameters.columns / 2) Set(West) else Set(East)
      } else if (one.column > other.column) {
        if (one.column - other.column >= parameters.columns / 2) Set(East) else Set(West)
      } else Set()

      ns ++ ew
    }
  }

  def tile(aim: CardinalPoint) = new {
    def of(tile: Tile) = {
      aim match {
        case North => tile.copy(row = if (tile.row == 0) parameters.rows - 1 else tile.row - 1)
        case South => tile.copy(row = (tile.row + 1) % parameters.rows)
        case East => tile.copy(column = (tile.column + 1) % parameters.columns)
        case West => tile.copy(column = if (tile.column == 0) parameters.columns - 1 else tile.column - 1)
      }
    }
  }

  // TODO finish
  def tilesWithinRadius(radiusSquared: Int) = new {
    private val radiusNotSquared = (math.ceil(math.sqrt(radiusSquared))).toInt
    def of(tile: Tile): Set[Tile] = {
      // to save on calculations, first make a bounding box then filter down
      val numColumns = parameters.columns
      val columnsToEast = ((tile.column + 1) to (tile.column + radiusNotSquared)) map { unwrappedColumn =>
        unwrappedColumn % numColumns
      }
      val columnsToWest = ((tile.column - radiusNotSquared) to (tile.column - 1)) map { unwrappedColumn =>
        val x = unwrappedColumn % numColumns
        if (x < 0) numColumns + x else x
      }
      val numRows = parameters.rows
      val rowsToNorth = ((tile.row - radiusNotSquared) to (tile.row - 1)) map { unwrappedRow =>
        val y = unwrappedRow % numRows
        if (y < 0) numRows + y else y
      }
      val rowsToSouth = ((tile.row + 1) to (tile.row + radiusNotSquared)) map { unwrappedRow =>
        unwrappedRow % numRows
      }
      
      val tilesInBox = for (
        x <- Set() ++ columnsToEast ++ columnsToWest + tile.column;
        y <- Set() ++ rowsToNorth ++ rowsToSouth + tile.row
      ) yield Tile(x, y)

      tilesInBox.filter{
        distanceFrom(tile).to(_) <= radiusSquared         
      }.toSet
    }
  } 
}

