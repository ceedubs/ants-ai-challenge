trait TileContext {
  def tile: Tile
  def adjacentTiles: Set[Tile]

  override def toString = tile.toString
}

