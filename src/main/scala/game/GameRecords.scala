package game

import player._


/**
  * This class is in charge of handling statistics about the entire game.
  * @param _player1 One of the player involved in the game
  * @param _player2 The second player involved in the game
  */
case class GameRecords (private val _player1: Player, private val _player2: Player){
  def player1: Player = this._player1
  def player2: Player = this._player2

  def finalScore(): String = player1.name + "; " + player1.score + "; " + player2.name + "; " + player2.score

}
