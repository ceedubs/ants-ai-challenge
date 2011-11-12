object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {

    // Your logic goes here.
    // for example ...

    val nextTurnMyAnts: scala.collection.mutable.Map[Tile, MyAnt] = scala.collection.mutable.HashMap()
    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values
    ants.flatMap{ant =>
      // for this ant, find the first direction which is not water, if any
      val direction = directions.find{aim =>
        val targetTile = game.tile(aim).of(ant.tile)
        !game.board.water.contains(targetTile) && !nextTurnMyAnts.contains(targetTile)
      }
      // convert this (possible) direction into an order for this ant
      direction.map{d =>
        println(d)
        nextTurnMyAnts += game.tile(d).of(ant.tile) -> ant
        println()
        println("tile to move to: " + game.tile(d).of(ant.tile))
        println("ant that will move there: " + ant)
        Order(ant.tile, d)
      }
    }.toSet
  }
}
