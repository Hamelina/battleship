package game

import config._
import iohandler._
import player.{AIs, Player}
import ship.{Cell, Ship}

import scala.annotation.tailrec

/**
  * This class is an utility in which some of the settings of the game are stored and it provides functions to display.
  */
case class Utility(private val _game: Game) {

  //the current game
  def game: Game = this._game
}

/**
  * This object corresponds to the
  */
object Utility {

  //an ASCII character used to print a grid
  val BLOCK = "██"

  /**
    * The different status of a square in a grid.
    */
  val OCCUPIED_STATUS = "occupied"
  val MISSED_STATUS = "missed"
  val HIT_STATUS = "hit"
  val NOT_TARGETED = "not targeted"



  /**
    * Asks a user to shoot
    * @param playerName The name of the player
    * @return The cell given by the user, corresponds to the coordinates of a square to target.
    */
  @tailrec
  def askUserToShoot(playerName: String): Cell = {

    Display.printAskForTarget(playerName)
    val target = Input.getShootCoordinates
    val coordinates = target.split(" ")
    coordinates.size match {
      case 2 =>{
        if (Cell.isValid(coordinates(0).toInt, coordinates(1).toInt)){
          Cell(coordinates(0).toInt, coordinates(1).toInt)
        }
        else {
          iohandler.Display.printFormatNotValid
          askUserToShoot(playerName)
        }
      }
      case _ => askUserToShoot(playerName)
    }
  }

  /**
    * This function returns a player who is being targeted
    * @param opponent The player whoose square is being targeted
    * @param x The coordinates on x-axis of the target square
    * @param y The coordinates on y-axis of the target square
    * @return Returns the player who has been targeted after a shoot.
    */
  def shoot(opponent: Player, x: Int, y: Int ): Player = {
    val ships: List[Ship] = opponent.fleet
    val cell: List[Cell] = ships.flatMap(x => x.cells)
    if (cell.contains(Cell(x, y))) {
      val fleet: List[Ship] = opponent.fleet.map(e => {
        if (e.cells.size > Ship.updatedShip(Cell(x, y), e).cells.size) {
          Display.printShipSunk
        }
        Ship.updatedShip(Cell(x, y), e)
      })
      val grid: Grid = Grid.updateGrid(opponent.gridStates, x, y, Utility.HIT_STATUS)
      Display.printTargetHit
      opponent.copy(_gridStates = grid, _fleet = fleet, _isTurnToPlay = true)
    }
    else {
      Display.printTargetMissed
      val grid2: Grid = Grid.updateGrid(opponent.gridStates, x, y, Utility.MISSED_STATUS)
      opponent.copy(_gridStates = grid2, _isTurnToPlay = true)
    }
  }



  /**
    * Asks a user to enter some information
    * @param shipFormat A list of tuple that corresponds to a list of number and size of ship that needs to be placed on the grid so that the game can begin.
    * @param fleet The list of initial existing cells corresponding to the initial existing fleet
    * @param player The player who is asked to place his/her/its ships.
    * @return A list of ships that corresponds to the coordinates of the ship the the player.
    */
  @tailrec
  def askUserForShipSettings(shipFormat: List[List[Int]], fleet: List[List[Cell]], player: Player): List[Ship] = {

    //there is no ship left to place
    if (shipFormat.isEmpty) {
      fleet.map(x => Ship(x.size, x))
    }

      //there is still ship left to place and its number is 0
    else if (1 > shipFormat.head(0)){
      askUserForShipSettings(shipFormat.tail, fleet, player)
    }
    //there is still at least one ship left to place
    else
    {
      Display.printAskShipToUser(player.name, shipFormat.head(1))
      val settings: String = Input.startingPoint
      val startingPoint: Array[String] = settings.split(" ")
      startingPoint.size match {
        case 3 => {

          val l2 = Ship.createCellsList(size = shipFormat.head(1), direction = startingPoint(2), Cell(startingPoint(0).toInt, startingPoint(1).toInt) :: Nil)
          val isValidShip: Boolean = Ship.isValid(x = startingPoint(0).toInt, y = startingPoint(1).toInt, shipFormat.head(1), direction = startingPoint(2))

          //if the settings entered for the ship to create are not superposed to the the settings of the existing fleet
          if (!isValidShip) {
            Display.printStartPositionNotValid
            askUserForShipSettings(shipFormat, fleet = fleet, player)
          }
          //if the inputs are valid
          else if (isValidShip && !Ship.hasAtLeastOneElement(l1 = fleet, l2 = l2))  {
            askUserForShipSettings(shipFormat.updated(0, shipFormat.apply(0).updated(0, shipFormat.head(0)-1)),fleet = l2+:fleet, player)
          }
          else {
            Display.printCoordinatesNotValid
            askUserForShipSettings(shipFormat, fleet = fleet, player)
          }
        }
        //the input is identified as a quit
        case _ => {
          Display.printCoodinatesNotCorrect
          askUserForShipSettings(shipFormat, fleet, player)
        }
      }
    }
  }

  /**
    * This function return whether a square targeted is a hit or not
    * @param fleet The current fleet that corresponds to the current occupied squares.
    * @param cell The coordinates of a square we need to check.
    * @return A boolean equals to true if the given cell is occupied, otherwise false.
    */
  def isHit(fleet: List[Ship], cell: Cell): Boolean = {
    if (fleet.isEmpty)  false
    else if (fleet.head.cells.contains(cell)) true
    else isHit(fleet.tail, cell)
  }

  /**
    * Initialize a round involving two player.
    * @param looser Either the looser last round or The player who starts the round.
    * @param winner Either the winner of the last round or the one who got to play in second position.
    * @return
    */
  def initializeRound(looser: Player, winner: Player): Game = {
    val listShipPlayer1: List[Ship] = Utility.askForShipSettings(Config.NUMBER_AND_SIZE_OF_SHIPS, Nil, looser)
    val gridPlayer1: Grid = Grid.initializeGridFromFleet(listShipPlayer1, Config.SIZE
    )
    val player1: Player = looser.copy(_fleet = listShipPlayer1, _gridStates = gridPlayer1, _isTurnToPlay = true, _toTarget = Nil)


    val listShipPlayer2: List[Ship] = Utility.askForShipSettings(Config.NUMBER_AND_SIZE_OF_SHIPS, Nil, winner)
    val gridPlayer2: Grid = Grid.initializeGridFromFleet(listShipPlayer2, Config.SIZE
    )
    val player2: Player = winner.copy(_fleet = listShipPlayer2, _gridStates = gridPlayer2, _isTurnToPlay=false,  _toTarget = Nil)

    Game(player1, player2)
  }

  /**
    * Ask the player to choose the mode of the game
    * @return A String that corresponds to the mode of the game.
    */
  def askPlayerMode(): String = {
    Display.printAskMode
    val mode = Input.mode
    mode match {
      case "1" => "AI mode"
      case "2" => "Human mode"
      case "3" => "AI vs AI"
      case _ => {
        Display.printModeIncorrect
        askPlayerMode
      }
    }
  }

  /**
    * Ask the user the level of the AI he/she wants to challenge
    * @return A string that corresponds to the level of AI the user wants to challenge: either 1, or 2 or 3. Otherwise the user is asked to give another number until it corresponds to the given selection.
    */
  def askForAILevel(): String = {
    Display.printAskLevel
    val level = Input.level
    level match {
      case "1" => "1"
      case "2" => "2"
      case "3" => "3"
      case _ => {
        Display.printLevelIncorrect
        askForAILevel
      }
    }
  }

  /**
    *Asks a player (whether an AI or a user) to place his/her/its ships.
    *
    * @param shipFormat A list of tuple that corresponds to a list of number and size of ship that needs to be placed on the grid so that the game can begin.
    * @param fleet The current fleet of the player
    * @param player The player who is asked to place his/her/its ships.
    * @return A list of ships that corresponds to the coordinates of the ship the the player.
    */
  def askForShipSettings(shipFormat: List[List[Int]], fleet: List[List[Cell]], player: Player): List[Ship] = {
    player.level match{
      case None => Utility.askUserForShipSettings(Config.NUMBER_AND_SIZE_OF_SHIPS, Nil, player)
      case _ => AIs.askAIForShipSetting(Config.NUMBER_AND_SIZE_OF_SHIPS, Nil, player)
    }
  }

  /**
    *Ask user to enter his/her name
    * @param order A String (either "first" of "second") that corresponds to the turn of the user whose name is asked.
    * @return A string that corresponds to the name of the user.
    */
  def askForName(order: String): String = {
    Display.printAskName(order)
    Input.name
  }

  /**
    * Ask player involved in a game (wheter an AI or a User) to shoot.
    * @param game The current game for which a player is asked to shoot.
    * @return A new game that corresponds to the game given in argument but after a player has shoot.
    */
  def askPlayerToShoot(game: Game): Game ={
    if (game.player1.isTurnToPlay) {
      game.player1.level match {
        case  None => {
          Display.clearScreen
          Display.printAnnounceMyGrid
          Display.printMyGrid(game)
          Display.printSeparator
          Display.printAnnounceOpponentGrid
          Display.printOpponentGrid(game)
          Display.printAskForTarget(game.player1.name)
          val cell: Cell = Utility.askUserToShoot(game.player1.name)
          val player1: Player = Utility.shoot(game.player2, cell.x, cell.y)
          val player2: Player = game.player1.copy(_isTurnToPlay = false)
          game.copy(_player1 = player1, _player2 = player2)
        }

        //level1
        case Some(1) => AIs.askAI1ToShoot(game.copy())

        //level2
        case Some(2) => {
          AIs.askAI3ToShoot(game.copy())

        }

        case _ => {
          AIs.askAI2ToShoot(game.copy())
        }

      }
    }
    else {
      //it is the turn of  the player2 to shoot
      game.player2 level match {
        case  None => {
          Display.clearScreen
          Display.printAnnounceMyGrid
          Display.printMyGrid(game)
          Display.printSeparator
          Display.printAnnounceOpponentGrid
          Display.printOpponentGrid(game)
          Display.printAskForTarget(game.player2.name)
          val cell: Cell = Utility.askUserToShoot(game.player2.name)
          val player1: Player = Utility.shoot(game.player1, cell.x, cell.y)
          Display.clearScreen
          val player2: Player = game.player1.copy(_isTurnToPlay = false)
          game.copy(_player1 = player1, _player2=player2)
        }

        //level1
        case Some(1) => AIs.askAI1ToShoot(game.copy(_player1 = game.player2, _player2 = game.player1))

        //level2
        case Some(2) => {
          AIs.askAI3ToShoot(game.copy(_player1 = game.player2, _player2 = game.player1))
        }

        //level3
        case _ => {
          AIs.askAI2ToShoot(game.copy(_player1 = game.player2, _player2 = game.player1))
        }

      }
    }


  }


  /**
    * Says whether or not a cell have already been targeted before (given a grid).
    * @param opponentGrid The grid on which the check is being made.
    * @param cell The coordinates of the cell to check.
    * @return A boolean equals to true if the cell given as an argument have already been targeted, otherwise it return false.
    */
  def haveBeenTargeted(opponentGrid: Grid, cell: Cell): Boolean = {
    //says whether or not a square have been targeted
    opponentGrid.gridStates(cell.y)(cell.x) match {
      case Utility.MISSED_STATUS => {
        true
      }
      case Utility.HIT_STATUS => {
        true
      }
      case _ => false
    }
  }

}
