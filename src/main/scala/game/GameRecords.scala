package game

import player._


/**
  * This class is in charge of handling statistics about the entire game.
  * @param _player1
  * @param _player2
  */
case class GameRecords (_player1: Player, _player2: Player){
  def player1: Player = _player1
  def player2: Player = _player2

  def finalScore(): String = player1.name + "; " + player1.score + "; " + player2.name + "; " + player2.score

}
