package player
import game._
import ship._

import scala.annotation.tailrec



/**
  *
  * @param _fleet The fleet owned by the player.
  * @param _name The name of the player. If the player is supposed to be an AI, the name is None.
  * @param _level If the player is an AI, it corresponds to the level of it. Otherwise it is None.
  * @param _score The number of game won by the Player
  * @param _isTurnToPlay A boolean that states if it is the player's turn to play. At the beginning of the game, it is always false.
  * @param _gridStates A grid that corresponds to a list of lists of String that describes the state of each cell (is used to display)
  */
case class Player (private val _fleet: List[Ship], private val _name: String, private val _level: Option[Int], private val _score: Int, private val _isTurnToPlay: Boolean, private val _gridStates: Grid, private val _hit: List[Cell], private val _testedDirection: List[String]){
  def fleet: List[Ship] = this._fleet
  def name: String = this._name
  def level: Option[Int] = this._level
  def score: Int = this._score
  def isTurnToPlay: Boolean = this._isTurnToPlay
  def gridStates: Grid = this._gridStates

  //memory of the number of hit(for AI3)
  def hit: List[Cell] = _hit

  //memory of the square tested for this direction
  def testedDirection: List[String] = _testedDirection
  /**
    * This function tells if this player has lost of not
    * @return Boolean equal to true if this player has lost, otherwise it returns false
    */
  def hasLost: Boolean = {
    this.numberOfShipLeft(fleet, 0)==0
  }
  /**
    * This function returns the number of ships left.
    * @param listShip The initial list of ships that one wants to know the number of ships left.
    * @param counter The number of ship which have not been sunk yet.
    * @return The number of ship which are not sunk yet.
    */
  @tailrec
  private def numberOfShipLeft(listShip: List[Ship], counter: Int): Int = {
    if (listShip.isEmpty)
      counter
    else if (listShip.head.isSunk) {
      numberOfShipLeft(listShip.tail, counter)
    }
    else numberOfShipLeft(listShip.tail, counter+1)
  }
}

object Player{



  //TODO
  //def fillGridByAIs(fleet: List[Ship], shipFormat: List[List[Int]]): List[Ship]

  //TODO
  //def targetIA1(grid: Grid): Cell
  //def targetIA2(grid: Grid): Cell
  //def targetIA3(grid: Grid): Cell


}
