class MyBotStrategy(game: Game, beliefState: BeliefState = BeliefState()) {
  val discountFactor = 0.7f // gamma
  val numActionsToLookAhead = 20
  val numValueIterations = 2 * numActionsToLookAhead

  def rewardOf(tile: Tile): Int = {
    val board = game.board
    if (board.myAnts.contains(tile)) {
      return Int.MinValue  
    } else if (board.water.contains(tile)) {
      return Int.MinValue
    } else if (board.food.contains(tile)) {
      return 500
    } else if (board.myHills.contains(tile)) {
      return Int.MinValue / 2
    } else if (board.enemyHills.contains(tile)) {
      return 500
    } else if (board.enemyAnts.contains(tile)) {
      return -200
    } else if (beliefState.explored.contains(tile)) {
      return -50
    } else { // unexplored
      return 5
    }
  }

  def calculatedUtilities: Map[Tile, Float] = {
    val tilesToConsider = tilesToConsiderIn(game, beliefState) 
    var tileUtilities: Map[Tile, Float] = Map()
    for (i <- 1 to numValueIterations) {
      val updatedTileUtilities: collection.mutable.Map[Tile, Float] = collection.mutable.Map()
      tilesToConsider.foreach{tile =>
        val adjacentTiles = AntMovement.allowedFor(MyAnt(tile)).in(game).map{_.to}.toSet - tile
        val maxAdjacentTileUtility = adjacentTiles.map{adjacentTile =>
          tileUtilities.getOrElse(adjacentTile, 0f)
        }.max
        val reward = rewardOf(tile)
        val updatedUtility = reward + discountFactor * maxAdjacentTileUtility
        updatedTileUtilities += tile -> updatedUtility
      }
      tileUtilities = updatedTileUtilities.toMap
    }
    tileUtilities
  }

  private def tilesToConsiderIn(game: Game, beliefState: BeliefState = BeliefState()): Set[Tile] = {
    val reachableTiles: collection.mutable.Set[Tile] = collection.mutable.Set()
    var leaves: Set[Tile] = game.board.myAnts.keySet
    for (i <- 1 to numActionsToLookAhead) {
      val newLeaves: collection.mutable.Set[Tile] = collection.mutable.Set()
      leaves.foreach{leafTile =>
        val allowedMovements = AntMovement.allowedFor(MyAnt(leafTile)).in(game)
        val adjacentTiles = allowedMovements.map{_.to}
        adjacentTiles.foreach{adjacentTile =>
          reachableTiles += adjacentTile
        }
        
        newLeaves ++= adjacentTiles.filter{adjacentTile =>
          !reachableTiles.contains(adjacentTile) && !leaves.contains(adjacentTile) 
        }
      }
      leaves = newLeaves.toSet
    }
    reachableTiles.toSet
  }

}

