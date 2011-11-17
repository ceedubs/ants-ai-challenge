case class AntMovement(ant: MyAnt, to: Tile, gameBeforeMove: Game) {

  def resultState: Game = {
    val board = gameBeforeMove.board
    val currentTile = ant.tile
    val updatedFood = board.food - currentTile // will be gone next round  
    // TODO handle other things going away next round
    val updatedMyAnts = board.myAnts.updated(to, ant) - currentTile
    val updatedBoard = board.copy(myAnts = updatedMyAnts, food = updatedFood)
    new GameInProgress(turn = gameBeforeMove.turn + 1, parameters = gameBeforeMove.parameters, board = updatedBoard)
  }

  def toOrders: Set[Order] = {
    val directionToMove = gameBeforeMove.directionFrom(ant.tile).to(to)
    directionToMove.map{d =>
      Order(ant.tile, d)
    }
  }
}

object AntMovement {

  def allowedFor(ant: MyAnt) = new {
    def in(game: Game): Set[AntMovement] = {
      val directions = List(North, East, South, West)
      val currentTile = ant.tile
      val adjacentTiles = directions.map{direction =>
        game.tile(direction).of(currentTile)
      }.toSet
      val reachableTiles = (adjacentTiles + currentTile).filter{tile =>
        !game.board.water.contains(tile)
      }
      val possibleNextTiles = reachableTiles.toSet
      possibleNextTiles.map{ nextTile =>
        AntMovement(ant = ant, to = nextTile, gameBeforeMove = game)
      }
    }
  }
}
