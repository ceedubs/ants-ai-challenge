class MyBotStrategy(game: Game, gameTracker: GameTracker) {
  private val discountFactor = 0.95f // gamma
  private val numAdditionalActionsToLookAhead = 1
  private val turnsPassedWeight = 2
  private val defaultTileUtility = 0f

  val tileContextFactory = new GameTrackingTileContextFactory(gameTracker, game)

  def rewardOf(tile: Tile): Int = {
    val board = game.board
/*    if (board.myAnts.contains(tile)) {
      return Int.MinValue  
    } else*/ if (board.water.contains(tile)) {
      return Int.MinValue
    } else if (board.food.contains(tile)) {
      return 500
    } else if (board.myHills.contains(tile)) {
      return Int.MinValue / 2
    } else if (board.enemyHills.contains(tile)) {
      return 2000
    } else if (gameTracker.enemyHills.contains(tile)) {
      return 1600
    } else if (board.enemyAnts.contains(tile)) {
      return -200
    } else {
//      val lastTurnViewed = gameTracker.tileToLastTurnViewed.getOrElse(tile, 0)
      val lastTurnVisited = gameTracker.tileToLastTurnVisited.getOrElse(tile, 0);
      // consider modifying so that this doesn't end up weighting more than other things if a lot of turns have passed since a square was visited
//      return turnsPassedWeight * (game.turn - lastTurnViewed)
      return turnsPassedWeight * (game.turn - lastTurnVisited)
    }
  }

  def calculatedUtilities: Map[Tile, Float] = {
//    val startTime = System.currentTimeMillis()
    val timeToStop = game.turnStartTime + game.parameters.turnTime / 2
//    println("timeToStop: " + timeToStop)
    var board = game.board
    var tileUtilities: Map[Tile, Float] = Map() ++ ((board.myAnts.keySet ++ board.food.keySet ++ gameTracker.enemyHills) map { _ -> defaultTileUtility })
    var tileContexts = tileUtilities.keySet.map{tileContextFactory contextOf _}.toSet
    var expectedIterationTime: Long = 100
    var iterationNum = 0
    while (System.currentTimeMillis() + expectedIterationTime < timeToStop) {
      val updatedTileContexts = collection.mutable.Set() ++ tileContexts
      val iterationStartTime = System.currentTimeMillis()
      iterationNum += 1
      val updatedTileUtilities: collection.mutable.Map[Tile, Float] = collection.mutable.Map()
      tileContexts.foreach{tileContext =>
        val adjacentTiles = tileContext.adjacentTiles
        val maxAdjacentTileUtility =
          if (adjacentTiles.isEmpty)
            Float.MinValue
          else
            adjacentTiles.map{adjacentTile =>
              tileUtilities.getOrElse(adjacentTile, defaultTileUtility)
            }.max
        
        val tile = tileContext.tile
        val reward = rewardOf(tile)
        val updatedUtility = reward + discountFactor * maxAdjacentTileUtility
//        println("reward: " + reward + "; updatedUtility: " + updatedUtility)
        updatedTileUtilities += tile -> updatedUtility
      }
      tileUtilities = updatedTileUtilities.toMap
      tileContexts = nextTilesToConsider(tileContexts)
      expectedIterationTime =  System.currentTimeMillis() - iterationStartTime
    }
//    val timeTook = System.currentTimeMillis - startTime
//    println("calcuatedUtilities took millis: " + timeTook)
//    println("num iterations: " + iterationNum)
    tileUtilities.withDefaultValue(0f)
  }

  private def nextTilesToConsider(initialSet: Set[TileContext]): Set[TileContext] = {
//    val startTime = System.currentTimeMillis()
    val board = game.board
    var leaves = initialSet
    val seenTiles = collection.mutable.Set() ++ (leaves map {_.tile})
    val tilesToConsider: collection.mutable.Set[TileContext] = collection.mutable.Set() ++ leaves
    for (i <- 1 to numAdditionalActionsToLookAhead) {
      val newLeaves: collection.mutable.Set[Tile] = collection.mutable.Set()
//      println("leaves: " + leaves)
      leaves.foreach{leafTileContext =>
        val adjacentTiles = leafTileContext.adjacentTiles
//        println("  adjacentTiles: " + adjacentTiles)
        adjacentTiles.foreach{adjacentTile =>
          if (!seenTiles.contains(adjacentTile)) {
            tilesToConsider += tileContextFactory.contextOf(adjacentTile)
            seenTiles += adjacentTile
            newLeaves += adjacentTile
          }
        }
        
      }
      if (i != numAdditionalActionsToLookAhead) {
        leaves = newLeaves.map{tile =>
          tileContextFactory.contextOf(tile)
        }.toSet
      }

    }
//    val timeTook = System.currentTimeMillis() - startTime;
//    println("tilesToConsiderIn took millis: " + timeTook)
    tilesToConsider.toSet
  }
}
