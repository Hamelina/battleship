package ship

import scala.annotation.tailrec

/**
  * This class corresponds to the current state of the ship a ship during a given game
 *
  * @param _typeName The type of the ship: either "Carrier" or "Battleship" or "Cruiser" or "Submarine" or "Destroyer"
  * @param _cells The cells currently occupied by the ship
  */
case class Ship (private val _typeName: String, private val _cells: List[Cell]){
  def typeName: String = this._typeName
  def cells: List[Cell] = this._cells

  /**
    * This function says if this ship is sunk or not
    * @return Boolean that corresponds to the state of the ship: either sunk or not
    */
  def isSunk: Boolean = cells.isEmpty

  /**
    * This function returns if this ship shares at least one cell with another ship or not.
    * @param ship2 The other ship
    * @return Boolean equal to true is the ship shares at least one cell with another ship, otherwise it returns false.
    */
  def isSuperposedTo(ship2: Ship): Boolean={

    /**
      * This function returns whether l1 containes at least one element of l2
      * @param l1 A list of cell(s)
      * @param l2 A list of cell(s)
      * @return A boolean equals to true if l1 contain at least one element of l2, otherwise it returns false
      */
    @tailrec
    def l1containsl2(l1: List[Cell], l2: List[Cell]): Boolean ={
      if (l2.isEmpty)
        false
      else if (l1.contains(l2.head))
            true
          else l1containsl2(l1, l2.tail)
    }
    if (cells.contains(ship2.cells.head)) true
    else l1containsl2(cells, ship2.cells.tail)
  }
}

object Ship{

  //given a cell for the beginning, a name, a direction and a size, render the corresponding ship
  /**
    * This function create a ship given a starting point, its type, its size and the direction in which it has to be built.
    * @param startPoint The starting cell from which the ship is built.
    * @param shipType The type of the ship.
    * @param shipSize The size of the ship.
    * @param direction The direction to which the ship will be constructed.
    * @return The expected ship.
    */

  def createShip (startPoint: Cell, shipType: String, shipSize: Int, direction: String): Ship = {

    /**
      * This function concatenates cells into a given list of cells.
      * @param size The size of the list of cells
      * @param direction The direction to which the list of cells will be constructed
      * @return A list of cells corresponding to a the coordinates occupied by a ship
      */
    @tailrec
    def createCellsList(size: Int, direction: String, list: List[Cell]): List[Cell] = {
      if (size==0)
        list
      else{
        direction match {
          case "up" => createCellsList (size = size-1, direction = direction, list = list.head.copy(_x= list.head.x + 1)::list)

          case "down" => createCellsList (size = size-1, direction = direction, list = list.head.copy(_x= list.head.x-1)::list)

          case "left" => createCellsList (size = size-1, direction = direction, list = list.head.copy(_y=list.head.y-1)::list)

          case "right" => createCellsList (size = size-1, direction = direction, list = list.head.copy(_y=list.head.y+1)::list)
        }
      }
    }
    new Ship(_typeName = shipType, _cells=createCellsList(shipSize-1, direction, List(startPoint)))
  }

  /**
    * This function returns if whether a ship can or cannot be built on an empty grid
    * @param x The row on which the ship is wanted to be built.
    * @param y The column in which the ship is wanted to be built.
    * @param size The size of the wanted ship.
    * @param direction The direction of the wanted ship.
    * @return A boolean equals to true if ship fits in an empty grid.
    */
  def isValid(x: Int, y:Int , size: Int, direction: String): Boolean = {

    Cell.isValid(x = x, y = y) && (direction match {

      case "up" => Cell.isValid(x = x, y = y - size + 1)

      case "down" => Cell.isValid(x = x, y = y + size - 1)

      case "left" => Cell.isValid(x = x - size + 1, y = y)

      case "right" => Cell.isValid(x = x + size - 1, y = y)
      case _ => false
    })
  }
}


