object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  var gameTracker = GameTracker()

  def ordersFrom(game: Game): Set[Order] = {
    gameTracker = updatedGameTracker(game, gameTracker)
    val strategy = new MyBotStrategy(game, gameTracker)
    val tileToUtility = strategy.calculatedUtilities
//    println("tileToUtility: " + tileToUtility)
    var updatedGame = game
    val tileContextFactory = new GameTrackingTileContextFactory(gameTracker, updatedGame)
    game.board.myAnts.values.flatMap{myAnt =>
      val adjacentTiles = tileContextFactory.contextOf(myAnt.tile).adjacentTiles

      val bestAdjacentTile = adjacentTiles.filter{tile =>
        // because ants might have moved around since we calculated utiities, we don't want to run into another ant
        !updatedGame.board.myAnts.contains(tile)
      }.+(myAnt.tile).maxBy{tile =>
        tileToUtility(tile)
      }
      val bestMovement = new AntMovement(myAnt, bestAdjacentTile, updatedGame)
      updatedGame = bestMovement.resultState
      bestMovement.toOrders
    }.toSet
  }

  // TODO update attributes other than explored
  private def updatedGameTracker(game: Game, previousGameTracker: GameTracker = GameTracker()) = {
//    val startTime = System.currentTimeMillis()
    val currentTurn = game.turn
    val board = game.board
    val myAntTiles = board.myAnts.keySet
    val visitedUpdates = myAntTiles.map{ tile =>
      tile -> currentTurn
    }
    val updatedTileVisits = previousGameTracker.tileToLastTurnVisited ++ visitedUpdates

    
    val tilesWithinViewRadius = game.tilesWithinRadius(game.parameters.viewRadius)
    val myAntToViewableNew = myAntTiles filterNot {
      previousGameTracker.tileToVisibleTiles.contains(_)
    } map { myAntTile =>
      myAntTile -> tilesWithinViewRadius.of(myAntTile)
    }
    
    val currentlyViewableTiles = myAntToViewableNew.map{_._2}.flatten
    val updatedTileViews = previousGameTracker.tileToLastTurnViewed ++ currentlyViewableTiles.map{ _ -> currentTurn }
    val updatedTileToViewable = previousGameTracker.tileToVisibleTiles ++ myAntToViewableNew

    val adjacentUpdates = currentlyViewableTiles.filterNot{
      previousGameTracker.tileToAdjacentReachableTiles.contains(_)
    }.map{ tile: Tile =>
      val adjacentTiles = AntMovement.allowedFor(MyAnt(tile)).in(game).map(_.to) - tile
      if (adjacentTiles forall {currentlyViewableTiles contains _})
        Some(tile -> adjacentTiles)
      else
        None
    }.flatten
    
    val updatedAdjacentTiles = previousGameTracker.tileToAdjacentReachableTiles ++ adjacentUpdates
//    val timeTook = System.currentTimeMillis() - startTime
//    println("updatedGameTracker took millis: " + timeTook)

    val updatedEnemyHills = previousGameTracker.enemyHills.filter{ hillFromMemory =>
      board.enemyHills.contains(hillFromMemory) || !currentlyViewableTiles.contains(hillFromMemory)
    } ++ board.enemyHills.keySet

    previousGameTracker.copy(tileToLastTurnVisited = updatedTileVisits, tileToLastTurnViewed = updatedTileViews, tileToVisibleTiles = updatedTileToViewable, tileToAdjacentReachableTiles = updatedAdjacentTiles, enemyHills = updatedEnemyHills)
  }

}
