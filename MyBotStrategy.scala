class MyBotStrategy(game: Game, gameTracker: GameTracker) {
  val discountFactor = 0.95f // gamma
  val numActionsToLookAhead = 15
  val numValueIterations = 2 * numActionsToLookAhead
  val turnsPassedWeight = 2

  val tileContextFactory = new GameTrackingTileContextFactory(gameTracker, game)

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
      return turnsPassedWeight * (game.turn - lastTurnVisited )
    }
  }

  def calculatedUtilities: Map[Tile, Float] = {
//    val startTime = System.currentTimeMillis()
    val tilesToConsider = tilesToConsiderIn(game) 
    var tileUtilities: Map[Tile, Float] = Map()
    for (i <- 1 to numValueIterations) {
      val updatedTileUtilities: collection.mutable.Map[Tile, Float] = collection.mutable.Map()
      tilesToConsider.foreach{tileContext =>
        val adjacentTiles = tileContext.adjacentTiles

        val maxAdjacentTileUtility =
          if (adjacentTiles.isEmpty)
            Float.MinValue
          else
            adjacentTiles.map{adjacentTile =>
              tileUtilities.getOrElse(adjacentTile, 0f)
            }.max
        
        val tile = tileContext.tile
        val reward = rewardOf(tile)
        val updatedUtility = reward + discountFactor * maxAdjacentTileUtility
        updatedTileUtilities += tile -> updatedUtility
      }
      tileUtilities = updatedTileUtilities.toMap
    }
//    val timeTook = System.currentTimeMillis - startTime
//    println("calcuatedUtilities took millis: " + timeTook)
    tileUtilities
  }

  private def tilesToConsiderIn(game: Game): Set[TileContext] = {
    val startTime = System.currentTimeMillis()
    val seenTiles: collection.mutable.Set[Tile] = collection.mutable.Set() ++ game.board.myAnts.keySet
    var leaves: Set[TileContext] = seenTiles.map{tile =>
      tileContextFactory.contextOf(tile)
    }.toSet
    val reachableTiles: collection.mutable.Set[TileContext] = collection.mutable.Set() ++ leaves
    for (i <- 1 to numActionsToLookAhead) {
      val newLeaves: collection.mutable.Set[Tile] = collection.mutable.Set()
      leaves.foreach{leafTileContext =>
        val adjacentTiles = leafTileContext.adjacentTiles
        adjacentTiles.foreach{adjacentTile =>
          seenTiles += adjacentTile
          reachableTiles += tileContextFactory.contextOf(adjacentTile)
        }
        
        if (i != numActionsToLookAhead) {
          newLeaves ++= adjacentTiles.filter{adjacentTile =>
            !seenTiles.contains(adjacentTile)
          }
        }
      }
      if (i != numActionsToLookAhead) {
        leaves = newLeaves.map{tile =>
          tileContextFactory.contextOf(tile)
        }.toSet
      }

    }
    val timeTook = System.currentTimeMillis() - startTime;
//    println("tilesToConsiderIn took millis: " + timeTook)
    reachableTiles.toSet
  }
}
