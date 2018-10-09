package game
import player._

/**
  * This class represent a game between 2 players.
  * @param _player1 The player number one.
  * @param _player2 The player number two.
  */
case class Game( private val _player1: Player,  private val _player2: Player){

  def player1: Player= this._player1
  def player2: Player= this._player2

  /**
    * Says if the game is over or not.
    * @return Boolean Returns true if a player wins, otherwise returns false.
    */
  def isGameOver: Boolean= {
    player1.hasLost || player2.hasLost
  }
}

/**
  * Companion of the class Game which stores the game settings and provided some static method.
  */
object Game{

  //The total number of round allowed
  val NUMBER_OF_TOTAL_ROUND: Int = 100

}
