object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  var beliefState = BeliefState()
  val strategy = new MyBotStrategy

  def ordersFrom(initialGame: Game): Set[Order] = {

    var updatedGame = initialGame
    println("food: " + initialGame.board.food)
    initialGame.board.myAnts.values.flatMap{myAnt =>
      println("ant tile: " + myAnt.tile)
      beliefState = beliefState.copy(explored = beliefState.explored + myAnt.tile)
      val allowedMovements = AntMovement.allowedFor(myAnt).in(game = updatedGame, beliefState = beliefState)
      val bestMovement = allowedMovements.maxBy{movement =>
        strategy.utilityOf(movement.to).in(updatedGame, beliefState)
      }
      updatedGame = bestMovement.resultState.game
      bestMovement.toOrders
    }.toSet
  }

}

class MyBotStrategy {
  val discountFactor = 0.8f // gamma
  val numActionsToLookAhead = 4

  def rewardOf(tile: Tile) = new {
    def in(game: Game, beliefState: BeliefState = BeliefState()): Int = {
      val board = game.board
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
  }

  def utilityOf(tile: Tile) = new {
    def in(game: Game, beliefState: BeliefState = BeliefState()): Float = {
      utility(1, 1, game, beliefState)
    }

    // TODO This is exponential in the number of actions to look ahead. It should be improved with Bellman equations.
    private def utility(currentIteration: Int, multiplier: Float, game: Game, beliefState: BeliefState): Float = {
      val reward = rewardOf(tile).in(game, beliefState)
      if (currentIteration == numActionsToLookAhead) {
        return reward
      }
      val allowedMovements = AntMovement.allowedFor(MyAnt(tile)).in(game)
      val utilityOfBestAction = allowedMovements.map{ movement =>
        utility(currentIteration + 1, discountFactor * multiplier, game, beliefState)
      }.max

      reward + multiplier * utilityOfBestAction
    }
  }

}
