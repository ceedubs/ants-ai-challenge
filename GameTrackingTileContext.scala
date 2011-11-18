class GameTrackingTileContextFactory(private val gameTracker: GameTracker, private val game: Game) {
  def contextOf(aTile: Tile) = new TileContext {
    def tile = aTile

    def adjacentTiles = adjacentReachableTiles(tile)
  }

  private def adjacentReachableTiles(tile: Tile): Set[Tile] = {
    if (gameTracker.tileToAdjacentReachableTiles.contains(tile)) {
      return gameTracker.tileToAdjacentReachableTiles(tile) 
    } else {
      val adjacentTiles = AntMovement.allowedFor(MyAnt(tile)).in(game).map(_.to) - tile
      return adjacentTiles.toSet
    }
  }
}
