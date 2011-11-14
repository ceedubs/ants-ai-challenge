object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  var beliefState = BeliefState()

  def ordersFrom(initialGame: Game): Set[Order] = {

    var updatedGame = initialGame
    println("food: " + initialGame.board.food)
    initialGame.board.myAnts.values.flatMap{myAnt =>
      println("ant tile: " + myAnt.tile)
      beliefState = beliefState.copy(explored = beliefState.explored + myAnt.tile)
      val allowedMovements = AntMovement.allowedFor(myAnt).in(game = updatedGame, beliefState = beliefState)
      val bestMovement = allowedMovements.maxBy{movement =>
        movement.utility
      }
      updatedGame = bestMovement.resultState
      bestMovement.toOrders
    }.toSet
  }

}
