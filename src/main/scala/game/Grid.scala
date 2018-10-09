package game

import ship._

import scala.annotation.tailrec

/**
  * This class corresponds to the grid of a player. It is used only to for the user interface.
  */
case class Grid (private val _gridStates: List[List[String]]){

  //The size of the grid
  def size: Int = gridStates.size

  //The tail of the grid
  def tail: List[List[String]] = gridStates.tail

  //The head of the grid
  def head: List[String] = gridStates.head

  //The state of each square. For example: gridState(0)(9) corresponds to the state of the cell (9,0)
  def gridStates: List[List[String]] = this._gridStates

}

/**
  * The companion of the class Grid. It store the static methods and attributes it.
  */
object Grid{

  //The size of the grid
  val SIZE = 10


  /**
    * Updates a grid given as an argument and given the position of the square to modify and its new status. It is used to update the grid of each player after a shoot.
    * @param grid The grid that needs to be updated
    * @param x The coordinate of the square on the x-axis that needs to be updated.
    * @param y The coordinate of the square on the y-axis that needs to be updated.
    * @param newStatus The new status of the square that needs to be updated.
    * @return A new Grid that have been updated.
    */
  def updateGrid(grid: Grid, x: Int, y: Int, newStatus: String): Grid = {
    val list = grid.gridStates
    grid.copy(_gridStates = list.updated(y, list.apply(y).updated(x, newStatus)))
  }

  /**
    * This function initialize a grid given a fleet and a size of grid
    * @param fleet The fleet from which that corresponds to the occupied cells
    * @param size The size of the grid
    * @return The initial grid of given the fleet
    */
  def initializeGridFromFleet(fleet: List[Ship], size: Int): Grid = {
    val defautList: List[List[String]] = List.fill(10)(List.fill(10)(Utility.NOT_TARGETED))
    val cells: List[Cell] = (fleet.map(x => x.cells)).flatMap(x => x)
    putShipOnGrid(cells, defautList)
  }
  @tailrec
  def putShipOnGrid(cells: List[Cell], statesList: List[List[String]]): Grid = {
    if (cells.isEmpty) new Grid(_gridStates = statesList)
    else {
      val cell = cells.head
      val newStateList = statesList.updated(cell.y, statesList.apply(cell.y).updated(cell.x, Utility.OCCUPIED_STATUS))
      putShipOnGrid(cells.tail, newStateList)
    }
  }
}




