package game

import ship.{Cell, Ship}

import scala.annotation.tailrec

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
  val limitY : Int = this.SIZE-1
  val NUMBER_OF_SHIP = 6

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




