package game
import player.Player
import ship._


case class Game( private val _player1: Player,  private val _player2: Player){

  def player1: Player= this._player1
  def player2: Player= this._player2

  /*def shoot(x: Int, y:Int): Game ={

  }*/
}

object Game{

  //1st player returned --> player, second--> opponent
  /**
    * This function returns a player who is being targeted
    * @param opponent The player whoose square is being targeted
    * @param x The coordinates on x-axis of the target square
    * @param y The coordinates on y-axis of the target square
    * @return Returns the player who has been targeted after a shoot.
    */
  def shoot(opponent: Player, x: Int, y: Int ): Player =
  {
    val ships: List[Ship] = opponent.fleet
    val cell: List[Cell] = ships.flatMap(x => x.cells)
    if (cell.contains(Cell(x,y))) {
      val fleet: List[Ship] = opponent.fleet.map(e => Ship.updatedShip(Cell(x, y), e))
      val grid: Grid = Grid.updateGrid(opponent.gridStates, x, y, Utility.HIT_STATUS)
      println("********HIIIIIIIIIIT")
      opponent.copy(_gridStates = grid, _fleet = fleet)
    }
    else{
      println("********MISSED")
      val grid2: Grid = Grid.updateGrid(opponent.gridStates, x, y, Utility.MISSED_STATUS)
      opponent.copy(_gridStates = grid2)
    }
  }
  def createPlayer(playerName: String, isTurnToPlay: Boolean): Player = {
    val fleet = Utility.askUserForShipSettings(shipFormat = Utility.NUMBER_AND_SIZE_OF_SHIPS, fleet = Nil, playerName= playerName)
    val grid = Utility.initializeGridFromFleet(fleet = fleet, size = Grid.SIZE)
    Player(fleet, Some(playerName), None, 0, isTurnToPlay, grid)
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
