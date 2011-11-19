case class GameTracker(tileToLastTurnVisited: Map[Tile, Int] = Map(),
                       tileToLastTurnViewed: Map[Tile, Int] = Map(),
                       tileToAdjacentReachableTiles: Map[Tile, Set[Tile]] = Map(),
                       tileToVisibleTiles: Map[Tile, Set[Tile]] = Map()) {

}
