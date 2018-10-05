package game
import player.Player

import scala.util.Random


case class Game( private val _player1: Player,  private val _player2: Player, private val _random: Random){

  def player1: Player= this._player1
  def player2: Player= this._player2
  def random: Random = this._random

  def isGameOver: Boolean= {
    player1.hasLost || player2.hasLost
  }

  /*def shoot(x: Int, y:Int): Game ={

  }*/
}

object Game{

  val NUMBER_OF_TOTAL_ROUND: Int = 100

  //TODO modify the function to print the results and write it in a CSV file




  def createPlayer(playerName: String, isTurnToPlay: Boolean): Player = {
    val fleet = Utility.askUserForShipSettings(shipFormat = Utility.NUMBER_AND_SIZE_OF_SHIPS, fleet = Nil, playerName= playerName)
    val grid = Grid.initializeGridFromFleet(fleet = fleet, size = Grid.SIZE)
    Player(fleet,playerName, None, 0, isTurnToPlay, grid, Nil, Nil)
  }



  /*@tailrec
  def isOccupied(fleet: List[List[Cell]], ship: List[Cell]): Boolean = {
    /**
      * This function returns whether l1 containes at least one element of l2
      * @param l1 A list of cell(s)
      * @param l2 A list of cell(s)
      * @return A boolean equals to true if l1 contain at least one element of l2, otherwise it returns false
      */
    @tailrec
    def l1containsl2(l1: List[Cell], l2: List[Cell]): Boolean ={
      if (l2.isEmpty || l1.isEmpty)
        false
      else if (l1containsl2(l1, l2.head))
        true
      else l1containsl2(l1, l2.tail)
    }

    if (l1containsl2(fleet.head, ship)) {
      true
    }
    else
      l1containsl2(fleet.head, ship) && isOccupied(fleet = fleet.tail, ship)
  }*/


  /*def createFleet(List): List[List[Cells]] ={

  }*/


  /*
    position du bateau de taille 1 : if ((Cell.isValid(x,y) && Ship.isValidShip(Cell(x, y)) => create ship
    positions du bateau de taille 2 :
   */

  /*
  vérif if shi
   */



  /*
  à partir d'une flotte, crée une grille
   */

  /*
    def createInitialPlayer(fleet: List[Ship], name: Option[String], level: Option[Int], isTurnToPlay: Boolean, gridStates: Grid): Player = {
      Player(_fleet = fleet, _name = name, _level = level, _score = 0, _isTurnToPlay= isTurnToPlay, _gridStates = gridStates)
    }
   */


}
