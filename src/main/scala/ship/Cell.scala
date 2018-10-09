package ship

import config._

/**
  * This class represent a cell from the grid that can be hit
  * @param _x the position of the cell x-axis (here it is converted to Int be it is shown as letter(string) to the user
  * @param _y the position of the cell y-axis
  */
case class Cell (private val _x:Int, private val _y:Int){

  /**
    * Getter of the x coordinates
    * @return An Int corresponding to the x coordinate of the cell
    */
  def x: Int = this._x

  /**
    * Getter of the y coordinates
    * @return An Int corresponding to the y coordinate of the cell
    */
  def y: Int = this._y
}

/**
  * The companion of the class Cell
  */
object Cell {

  /**
    * This function returns if whether a point is inside the grid or not
    * @param x The coordonate on the x-axis of the cell that needs to be checked
    * @param y The coordonate on the y-axis of the cell that needs to be checked
    * @return Boolean equals to true if the cell is inside the grid, otherwise it equals to false
    */
  def isValid(x: Int, y: Int): Boolean = if ((x < 0) || (x >= Config.SIZE) || (y < 0) || (y >= Config.SIZE)) {
    false
  } else true



}
