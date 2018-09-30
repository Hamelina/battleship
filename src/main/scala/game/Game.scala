package game

import player.Player


case class Game( private val _player1: Player,  private val _player2: Player){

  def player1: Player= this._player1
  def player2: Player= this._player2



}
