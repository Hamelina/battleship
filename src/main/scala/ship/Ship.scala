package ship

import scala.annotation.tailrec

/**
  * This class corresponds to the current state of the ship a ship during a given game
 *
  * @param _size The type of the original ship
  * @param _cells The cells currently occupied by the ship
  */
case class Ship (private val _size: Int, private val _cells: List[Cell]){
  def size: Int = this._size
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
    * Contraint: l2 ship2.cells.size <= this.cells.size
    */
  def isSuperposedTo(ship2: Ship): Boolean={
    if (Ship.l1containsl2(l1= cells, l2= ship2.cells)) true
    else Ship.l1containsl2(cells, ship2.copy().cells.tail)
  }
}

object Ship{

  //given a cell for the beginning, a name, a direction and a size, render the corresponding ship
  /**
    * This function creates a ship given a starting point, its type, its size and the direction in which it has to be built.
    * @param startPoint The starting cell from which the ship is built.
    * @param shipSize The size of the ship.
    * @param direction The direction to which the ship will be constructed.
    * @return The expected ship.
    */

  def createShipFromStartingPoint (startPoint: Cell, shipSize: Int, direction: String): Ship={
    new Ship(_size = this.createCellsList(shipSize, direction, List(startPoint)).size , _cells=this.createCellsList(shipSize, direction, List(startPoint)))
  }

  /*def createShipFromListOfCells( cellsList: List[Cell]): Ship = {
    val name = listCells.size match {
      case 1 => "Carrier"
      case 2 =>  "BattleShip"
      case 3 => "Cruiser"
      case 4 => "Submarine"
      case 5 => "Destroyer"
    }
    new Ship(_typeName = name, _cells = cellsList)
  }*/

  /**
    * This function concatenates cells into a given list of cells.
    * @param size The size of the list of cells, must be > 1
    * @param direction The direction to which the list of cells will be constructed
    * @return A list of cells corresponding to a the coordinates occupied by a ship
    * Constraint: when calling the method, list must contain at least one cell (the starting one)
    */
  @tailrec
  def createCellsList(size: Int, direction: String, list: List[Cell]): List[Cell] = {
    if (size<=1)
      list
    else{
      direction match {
        case "U" => createCellsList (size = size-1, direction = direction, list = list.head.copy(_y= list.head.y + 1)::list)

        case "D" => createCellsList (size = size-1, direction = direction, list = list.head.copy(_y= list.head.y-1)::list)

        case "L" => createCellsList (size = size-1, direction = direction, list = list.head.copy(_x=list.head.x-1)::list)

        case "R" => createCellsList (size = size-1, direction = direction, list = list.head.copy(_x=list.head.x+1)::list)
      }
    }
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

      case "U" => Cell.isValid(x = x, y = y - size + 1)

      case "D" => Cell.isValid(x = x, y = y + size - 1)

      case "L" => Cell.isValid(x = x - size + 1, y = y)

      case "R" => Cell.isValid(x = x + size - 1, y = y)
      case _ => false
    })
  }

  /**
    * This function returns whether l1 containes at least one element of l2
    * @param l1 A list of cell(s)
    * @param l2 A list of cell(s)
    * @return A boolean equals to true if l1 contain at least one element of l2, otherwise it returns false
    * Constraint: l2 must be shorter than or equal to l1
    */
  @tailrec
  def l1containsl2(l1: List[Cell], l2: List[Cell]): Boolean ={
    if (l2.isEmpty)
      false
    else if (l1.contains(l2.head))
      true
    else l1containsl2(l1, l2.tail)
  }


  /**
    * This function tests if a list of list of cells contains at least one cell from another list of cells
    */
  def hasAtLeastOneElement(l1: List[List[Cell]], l2: List[Cell]): Boolean = {
    val list: List[Cell] = l1.flatten(x => x)
    if (list.size> l2.size) l1containsl2(list, l2) else l1containsl2(l2, list)
  }

  def updatedShip(cell: Cell, ship: Ship): Ship = new Ship(ship.size,ship.cells.filter(x => x!=cell))



}


