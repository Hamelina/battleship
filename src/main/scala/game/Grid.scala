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
  val limitX: Int = Utility.GRID_LIMIT_X
  val limitY : Int= Utility.GRID_LIMIT_Y
}


