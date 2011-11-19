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
    game.board.myAnts.values.flatMap{myAnt =>
      val allowedMovements = AntMovement.allowedFor(myAnt).in(game = updatedGame)
      val bestMovement = allowedMovements.filter{movement =>
        // because ants might have moved around since we calculated utiities, we don't want to run into another ant
        val to = movement.to
        !updatedGame.board.myAnts.contains(to) || to == myAnt.tile
      }.maxBy{movement =>
        tileToUtility(movement.to)
      }
      updatedGame = bestMovement.resultState
      bestMovement.toOrders
    }.toSet
  }

  // TODO update attributes other than explored
  private def updatedGameTracker(game: Game, previousGameTracker: GameTracker = GameTracker()) = {
//    val startTime = System.currentTimeMillis()
    val currentTurn = game.turn
    val myAntTiles = game.board.myAnts.keySet
    val visitedUpdates = myAntTiles.map{ tile =>
      tile -> currentTurn
    }
    val updatedTileVisits = previousGameTracker.tileToLastTurnVisited ++ visitedUpdates

    
    val tilesWithinViewRadius = game.tilesWithinRadius(game.parameters.viewRadius)
    val viewable = myAntTiles map { tile =>
      (tile -> previousGameTracker.tileToVisibleTiles.getOrElse(tile, tilesWithinViewRadius of tile))
    }

    val updatedTileToViewable = previousGameTracker.tileToVisibleTiles ++ viewable
    val updatedTileViews = previousGameTracker.tileToLastTurnViewed ++ viewable.map{ _._1 -> currentTurn}

    val adjacentUpdates = myAntTiles.filterNot{
      previousGameTracker.tileToAdjacentReachableTiles.contains(_)
    }.map{tile =>
      val adjacentTiles = AntMovement.allowedFor(MyAnt(tile)).in(game).map(_.to) - tile
      (tile, adjacentTiles)
    }
    val updatedAdjacentTiles = previousGameTracker.tileToAdjacentReachableTiles ++ adjacentUpdates
    // TODO explored should really include all tiles that have been within visibility - not just tiles to which ants have actually moved
//    val timeTook = System.currentTimeMillis() - startTime
//    println("updatedGameTracker took millis: " + timeTook)
    previousGameTracker.copy(tileToLastTurnVisited = updatedTileVisits, tileToLastTurnViewed = updatedTileViews, tileToVisibleTiles = updatedTileToViewable, tileToAdjacentReachableTiles = updatedAdjacentTiles)
  }

}

