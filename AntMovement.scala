case class AntMovement(ant: MyAnt, to: Tile, gameBeforeMove: Game, beliefState: BeliefState = BeliefState()) {
  val discountFactor = 0.8f // gamma
  val numActionsToLookAhead = 4

  // TODO treat explored and unexplored tiles differently
  // TODO refactor hard-coding
  def reward: Int = {
    val tile = to
    val board = gameBeforeMove.board
    if (board.myAnts.contains(tile)) {
      return Int.MinValue  
    } else if (board.water.contains(tile)) {
      return Int.MinValue
    } else if (board.food.contains(tile)) {
      println("contains food")
      return 500
    } else if (board.myHills.contains(tile)) {
      return Int.MinValue / 2
    } else if (board.enemyHills.contains(tile)) {
      return 1000
    } else if (board.enemyAnts.contains(tile)) {
      return -200
    } else if (beliefState.explored.contains(tile)) {
      return -50
    } else { // unexplored
      return 5
    }
  }

  def resultState: Game = {
    val board = gameBeforeMove.board
    val currentTile = ant.tile
    val updatedFood = board.food - currentTile // will be gone next round  
    // TODO handle other things going away next round
    val updatedMyAnts = board.myAnts.updated(to, ant) - currentTile
    val updatedBoard = board.copy(myAnts = updatedMyAnts, food = updatedFood)
    new GameInProgress(turn = gameBeforeMove.turn + 1, parameters = gameBeforeMove.parameters, board = updatedBoard)
  }

  def utility: Float = {
    utility(1, 1)
  }

  // TODO This is exponential in the number of actions to look ahead. It should be improved with Bellman equations.
  private def utility(currentIteration: Int, multiplier: Float): Float = {
    if (currentIteration == numActionsToLookAhead) {
      return reward
    }
    val allowedMovements = AntMovement.allowedFor(ant).in(resultState)
    val utilityOfBestAction = allowedMovements.map{ movement =>
      movement.utility(currentIteration + 1, discountFactor * multiplier)
    }.max

    reward + multiplier * utilityOfBestAction
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
    def in(game: Game, beliefState: BeliefState = BeliefState()): Set[AntMovement] = {
      val directions = List(North, East, South, West)
      val currentTile = ant.tile
      val adjacentReachableTiles = directions.map{direction =>
        game.tile(direction).of(currentTile)
      }.filter{tile =>
        !game.board.water.contains(tile)
      }
      val possibleNextTiles = adjacentReachableTiles.toSet + currentTile
      possibleNextTiles.map{ nextTile =>
        AntMovement(ant = ant, to = nextTile, gameBeforeMove = game, beliefState = beliefState)
      }
    }
  }
}
