class MyBotStrategy(game: Game, gameTracker: GameTracker) {
  val discountFactor = 0.95f // gamma
  val numActionsToLookAhead = 10
  val numValueIterations = (1.5 * numActionsToLookAhead).toInt

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
    } else {
      val lastTurnVisited = gameTracker.tileToLastTurnVisited.getOrElse(tile, 0); 
      // consider modifying so that this doesn't end up weighting more than other things if a lot of turns have passed since a square was visited
      return game.turn - lastTurnVisited 
    }
  }

  def calculatedUtilities: Map[Tile, Float] = {
    val tilesToConsider = tilesToConsiderIn(game) 
    var tileUtilities: Map[Tile, Float] = Map()
    for (i <- 1 to numValueIterations) {
      val updatedTileUtilities: collection.mutable.Map[Tile, Float] = collection.mutable.Map()
      tilesToConsider.foreach{tile =>
        val adjacentTiles = AntMovement.allowedFor(MyAnt(tile)).in(game).map{_.to}.toSet - tile

        val maxAdjacentTileUtility =
          if (adjacentTiles.isEmpty)
            Float.MinValue
          else
            adjacentTiles.map{adjacentTile =>
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

  private def tilesToConsiderIn(game: Game): Set[Tile] = {
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

