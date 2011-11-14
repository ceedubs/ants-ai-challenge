case class BeliefState(explored: Set[Tile] = collection.immutable.HashSet[Tile](),
                       enemyAnts: Map[Tile, EnemyAnt] = Map(),
                       food: Map[Tile, Food] = Map(),
                       enemyHills: Map[Tile, EnemyHill] = Map()) {

  lazy val elements = enemyAnts ++ food ++ enemyHills

  def including[P <: Positionable](positionable: P) = positionable match {
      case enemy: EnemyAnt => this.copy(enemyAnts = this.enemyAnts.updated(enemy.tile, enemy))
      case crumb: Food => this.copy(food = this.food.updated(crumb.tile, crumb))
      case enemy: EnemyHill => this.copy(enemyHills = this.enemyHills.updated(enemy.tile, enemy))
    }
}

