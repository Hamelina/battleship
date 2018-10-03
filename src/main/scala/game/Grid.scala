package game

//the grid that represents the grid of a player
/**
  * This class corresponds to the grid of a player
  */
case class Grid (private val _gridStates: List[List[String]]){
  def size: Int = gridStates.size

  def tail: List[List[String]] = gridStates.tail

  def head: List[String] = gridStates.head

  def gridStates: List[List[String]] = this._gridStates

  //seek a given place in the grid --> a 0 becomes
}

object Grid{
  val SIZE = 10
  val limitX: Int = this.SIZE-1
  val limitY : Int= this.SIZE-1
  val NUMBER_OF_SHIP = 6

  def updateGrid(grid: Grid, x: Int, y: Int, newStatus: String): Grid = {
    val list = grid.gridStates
    grid.copy(_gridStates = list.updated(x, list.apply(x).updated(y, newStatus)))
  }

}




