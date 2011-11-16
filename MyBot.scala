object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  var beliefState = BeliefState()

  def ordersFrom(game: Game): Set[Order] = {
    beliefState = updatedBeliefState(game, beliefState)
    val strategy = new MyBotStrategy(game, beliefState)
    val tileToUtility = strategy.calculatedUtilities

    var updatedGame = game
    game.board.myAnts.values.flatMap{myAnt =>
      val allowedMovements = AntMovement.allowedFor(myAnt).in(game = updatedGame, beliefState = beliefState)
      val bestMovement = allowedMovements.filter{movement =>
        // because ants might have moved around since we calculated utiities, we don't want to run into another ant
        val to = movement.to
        !updatedGame.board.myAnts.contains(to) || to == myAnt.tile
      }.maxBy{movement =>
        tileToUtility(movement.to)
      }
      updatedGame = bestMovement.resultState.game
      bestMovement.toOrders
    }.toSet
  }

  // TODO update attributes other than explored
  private def updatedBeliefState(game: Game, previousBeliefState: BeliefState = BeliefState()) = {

    // TODO explored should really include all tiles that have been within visibility - not just tiles to which ants have actually moved
    beliefState.copy(explored = beliefState.explored ++ game.board.myAnts.keySet)
  }

}

