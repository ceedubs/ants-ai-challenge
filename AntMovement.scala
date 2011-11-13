case class AntMovement(ant: MyAnt, to: Tile, gameBeforeMove: Game) {
  // TODO treat explored and unexplored tiles differently
  // TODO refactor hard-coding
  def reward: Int = {
    val tile = to
    val board = gameBeforeMove.board
    if (board.myAnts.contains(tile)) {
      println("contains myAnt")
      return Int.MinValue  
    } else if (board.water.contains(tile)) {
      println("contains water")
      return Int.MinValue
    } else if (board.food.contains(tile)) {
      println("contains food")
      return 5 
    } else if (board.myHills.contains(tile)) {
      println("contains myHill")
      return Int.MinValue / 2
    } else if (board.enemyHills.contains(tile)) {
      println("contains enemyHill")
      return 10
    } else if (board.enemyAnts.contains(tile)) {
      println("contains enemyAnt")
      return -2
    } else {
      println("contains none of the above")
      return -1
    }
  }

  def resultState: Game = {
    val board = gameBeforeMove.board
    val currentTile = ant.tile
    val updatedFood = board.food - currentTile // will be gone next round  
    // TODO handle other things going away next round
    val updatedMyAnts = board.myAnts.updated(to, ant) - currentTile
    val updatedBoard = board.copy(myAnts = updatedMyAnts)
    new GameInProgress(turn = gameBeforeMove.turn + 1, parameters = gameBeforeMove.parameters, board = updatedBoard)
  }

  // TODO temp filler - need to implement
  def utility: Long = {
    reward
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
      val adjacentReachableTiles = directions.map{direction =>
        game.tile(direction).of(currentTile)
      }.filter{tile =>
        !game.board.water.contains(tile)
      }
      val possibleNextTiles = adjacentReachableTiles.toSet + currentTile
      possibleNextTiles.map{ nextTile =>
        AntMovement(ant, nextTile, game)
      }
    }
  }
}
