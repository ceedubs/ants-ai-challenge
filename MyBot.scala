object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(initialGame: Game): Set[Order] = {

    var updatedGame = initialGame
    initialGame.board.myAnts.values.flatMap{myAnt =>
      val allowedMovements: Set[AntMovement] = AntMovement.allowedFor(myAnt).in(updatedGame) 
      val bestMovement = allowedMovements.maxBy{movement =>
        movement.utility
      }
      updatedGame = bestMovement.resultState
      bestMovement.toOrders
    }.toSet
  }

}
