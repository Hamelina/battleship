package player
import game._
import ship._

import scala.annotation.tailrec
import scala.util.Random



/**
  * A class that corresponds to a Player (either AI or User)
  * @param _fleet The fleet owned by the player.
  * @param _name The name of the player. If the player is supposed to be an AI, the name is None.
  * @param _level If the player is an AI, it corresponds to the level of it. Otherwise it is None.
  * @param _score The number of game won by the Player
  * @param _isTurnToPlay A boolean that states if it is the player's turn to play. At the beginning of the game, it is always false.
  * @param _gridStates A grid that corresponds to a list of lists of String that describes the state of each cell (is used to display)
  */
case class Player (private val _fleet: List[Ship], private val _name: String, private val _level: Option[Int], private val _score: Int, private val _isTurnToPlay: Boolean, private val _gridStates: Grid, private val _toTarget: List[Cell], private val _random: Option[Random]) {
  def fleet: List[Ship] = this._fleet

  def name: String = this._name

  def level: Option[Int] = this._level

  def score: Int = this._score

  def isTurnToPlay: Boolean = this._isTurnToPlay

  def gridStates: Grid = this._gridStates

  def random: Option[Random] = this._random

  //memory of the number of hit(for AI3)
  def toTarget: List[Cell] = this._toTarget


  /**
    * This function tells if this player has lost of not
    *
    * @return Boolean equal to true if this player has lost, otherwise it returns false
    */
  def hasLost: Boolean = {
    this.numberOfShipLeft(fleet, 0) == 0
  }

  /**
    * This function returns the number of ships left.
    *
    * @param listShip The initial list of ships that one wants to know the number of ships left.
    * @param counter  The number of ship which have not been sunk yet.
    * @return The number of ship which are not sunk yet.
    */
  @tailrec
  private def numberOfShipLeft(listShip: List[Ship], counter: Int): Int = {
    if (listShip.isEmpty)
      counter
    else if (listShip.head.isSunk) {
      numberOfShipLeft(listShip.tail, counter)
    }
    else numberOfShipLeft(listShip.tail, counter + 1)
  }


  /**
    * This function returns whether a ship is sunk or not
    *
    * @param fleet The fleet that needs to be checked
    * @param cell The cell corresponding to the square targeted
    * @return Boolean equals to true if the cell hit a ship, otherwise returns false.
    */
  def isShipSunk(fleet: List[Ship], cell: Cell): Boolean = {
    if (fleet.isEmpty) false
    else if (fleet.head.cells.contains(cell)) true
    else isShipSunk(fleet.tail, cell)
  }
}


