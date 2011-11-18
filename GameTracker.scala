case class GameTracker(tileToLastTurnVisited: Map[Tile, Int] = Map(),
                       tileToAdjacentReachableTiles: Map[Tile, Set[Tile]] = Map()) {

}
