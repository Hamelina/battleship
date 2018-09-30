package player
import game.Grid
import ship._



/**
  *
  * @param _fleet The fleet owned by the player.
  * @param _name The name of the player. If the player is supposed to be an AI, the name is None.
  * @param _level If the player is an AI, it corresponds to the level of it. Otherwise it is None.
  * @param _score The number of game won by the Player
  * @param _isTurnToPlay A boolean that states if it is the player's turn to play. At the beginning of the game, it is always false.
  * @param _gridStates A grid that corresponds to a list of lists of String that describes the state of each cell (is used to display)
  */
case class Player (private val _fleet: List[Ship], private val _name: Option[String], private val _level: Option[Int], private val _score: Int, private val _isTurnToPlay: Boolean, private val _gridStates: Grid){
  def fleet: List[Any] = this._fleet
  def name: Option[String] = this._name
  def level: Option[Int] = this._level
  def score: Int = this._score
  def isTurnToPlay: Boolean = this._isTurnToPlay
  def gridStates: Grid = this._gridStates

  //is hittable (x,y)
  //returns either the x,y is hittable or not


  /*def hit(x: Int, y: Int, player: Player): Player= {
    //return the new Player with the modified fleet
  }*/
}
