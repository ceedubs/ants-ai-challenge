object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  var gameTracker = GameTracker()

  def ordersFrom(game: Game): Set[Order] = {
    gameTracker = updatedGameTracker(game, gameTracker)
    val strategy = new MyBotStrategy(game, gameTracker)
    val tileToUtility = strategy.calculatedUtilities
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
    val currentTurn = game.turn
    val updates = game.board.myAnts.keySet.map{tile =>
      (tile, currentTurn)
    }
    val updatedTileVisits = previousGameTracker.tileToLastTurnVisited ++ updates
    // TODO explored should really include all tiles that have been within visibility - not just tiles to which ants have actually moved
    previousGameTracker.copy(tileToLastTurnVisited = updatedTileVisits)
  }

}

