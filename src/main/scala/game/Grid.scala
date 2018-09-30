package game

//the grid that represents the grid of a player
/**
  * This class corresponds to the grid of a player
  */
case class Grid (_size: Int, _gridStates: List[List[String]]){
  def size: Int = _size
  def gridStates: List[List[String]] = _gridStates

  //seek a given place in the grid --> a 0 becomes
}

object Grid{
  val limitX = 10
  val limitY = 10
}


