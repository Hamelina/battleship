package game

import iohandler._
import player.Player
import ship.{Cell, Ship}

import scala.annotation.tailrec
import scala.util.Random

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

  //Size and number of the different ships allowed during the game
  val NUMBER_AND_SIZE_OF_SHIPS: List[List[Int]] = List(List(1,1),List(1,2),List(2,3),List(1,4),List(1,5))
  //Maximum round that can be player between two players.
  val NUMBER_OF_TOTAL_ROUND: Int = 100

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
    * Ask an AI to place ships.
    * @param shipFormat
    * @param fleet
    * @param player
    * @return A list of ships
    */
  @tailrec
  def askAIForShipSetting(shipFormat: List[List[Int]], fleet: List[List[Cell]], player: Player): List[Ship] = {
    //there is no ship left to place
    if (shipFormat.isEmpty) {
      fleet.map(x => Ship(x.size, x))
    }
    else if (1 > shipFormat.head(0)){
      askAIForShipSetting(shipFormat.tail, fleet, player)
    }
    //there is still at least one ship left to place
    else
    {
      //we arbitrary set the direction to R
      val r: Random = player.random.get
      val x = r.nextInt(Grid.SIZE)
      val y = r.nextInt(Grid.SIZE)
      val l2 = Ship.createCellsList(size = shipFormat.head(1), direction = "R", Cell(x, y) :: Nil)
      val isValidShip: Boolean = Ship.isValid(x = x, y = y, shipFormat.head(1), direction = "R")

      //if the settings generated for the ship to create are not superposed to the the settings of the existing fleet
      if (!isValidShip) {
        askAIForShipSetting(shipFormat, fleet = fleet, player.copy(_random = Option(r)))
      }
      //if the inputs are valid
      else if (isValidShip && !Ship.hasAtLeastOneElement(l1 = fleet, l2))  {
        askAIForShipSetting(shipFormat.updated(0, shipFormat.apply(0).updated(0, shipFormat.head(0)-1)),fleet = l2+:fleet, player.copy(_random = Option(r)))
      }
      else {
        askAIForShipSetting(shipFormat, fleet, player.copy(_random = Option(r)))
      }
    }
  }



  /**
    * Asks a user to enter some information
    * @param shipFormat A list of tuple that corresponds to a list of number and size of ship that needs to be placed on the grid so that the game can begin.
    * @param fleet The list of initial existing cells corresponding to the initial existing fleet
    * @return A list of ships that corresponds the possible ships created by the input of the user
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
  def isShipSunk(fleet: List[Ship], cell: Cell): Boolean = {
    if (fleet.isEmpty)  false
    else if (fleet.head.cells.contains(cell)) true
    else isShipSunk(fleet.tail, cell)
  }

  def initializeRound(looser: Player, winner: Player): Game = {
    val listShipPlayer1: List[Ship] = Utility.askForShipSettings(NUMBER_AND_SIZE_OF_SHIPS, Nil, looser)
    val gridPlayer1: Grid = Grid.initializeGridFromFleet(listShipPlayer1, Grid.SIZE)
    val player1: Player = looser.copy(_fleet = listShipPlayer1, _gridStates = gridPlayer1, _isTurnToPlay = true, _currentDirection = None, _targeted = Nil)


    val listShipPlayer2: List[Ship] = Utility.askForShipSettings(Utility.NUMBER_AND_SIZE_OF_SHIPS, Nil, winner)
    val gridPlayer2: Grid = Grid.initializeGridFromFleet(listShipPlayer2, Grid.SIZE)
    val player2: Player = winner.copy(_fleet = listShipPlayer2, _gridStates = gridPlayer2, _isTurnToPlay=false, _currentDirection = None, _targeted = Nil)

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

  def askForShipSettings(shipFormat: List[List[Int]], fleet: List[List[Cell]], player: Player): List[Ship] = {
    player.level match{
      case None => Utility.askUserForShipSettings(Utility.NUMBER_AND_SIZE_OF_SHIPS, Nil, player)
      case _ => Utility.askAIForShipSetting(Utility.NUMBER_AND_SIZE_OF_SHIPS, Nil, player)
    }
  }

  def askForName(order: String): String = {
    Display.printAskName(order)
    Input.name
  }

  def askPlayerToShoot(game: Game): Game ={

    /*Display.clearScreen
    Display.printAnnounceMyGrid
    Display.printMyGrid(game)
    Display.printSeparator
    Display.printAnnounceOpponentGrid
    Display.printOpponentGrid(game)*/
    if (game.player1.isTurnToPlay) {
      game.player1.level match {
        case  None => {
          Display.printAskForTarget(game.player1.name)
          val cell: Cell = Utility.askUserToShoot(game.player1.name)
          val player1: Player = Utility.shoot(game.player2, cell.x, cell.y)
          val player2: Player = game.player1.copy(_isTurnToPlay = false)
          game.copy(_player1 = player1, _player2 = player2)
        }

        //TODO for the  AI
        //level1
        case Some(1) => askAI1ToShoot(game.copy())
        //level2
        case Some(2) => {
          askAI2ToShoot(game.copy())
        }
        //level3
        case _ => {
          askAI3ToShoot(game.copy())
        }

      }
    }
    else {
      //it ut to the player2 to shoot
      game.player2 level match {
        case  None => {
          Display.printAskForTarget(game.player2.name)
          val cell: Cell = Utility.askUserToShoot(game.player2.name)
          val player1: Player = Utility.shoot(game.player1, cell.x, cell.y)
          Display.clearScreen
          val player2: Player = game.player1.copy(_isTurnToPlay = false)
          game.copy(_player1 = player1, _player2=player2)
        }

        //TODO pour les ia
        //level1
        case Some(1) => askAI1ToShoot(game.copy(_player1 = game.player2, _player2 = game.player1))
        //level2
        case Some(2) => {
          askAI2ToShoot(game.copy(_player1 = game.player2, _player2 = game.player1))
        }
        //level3
        case _ => {
          askAI3ToShoot(game.copy(_player1 = game.player2, _player2 = game.player1))
        }

      }
    }


  }



  //TODO continu for the AI
  def askAI1ToShoot(game: Game): Game = {
    val cell: Cell = Cell(game.player1.random.get.nextInt(Grid.SIZE), game.player1.random.get.nextInt(Grid.SIZE))
    val player1: Player = Utility.shoot(game.player2, cell.x, cell.y)
    Display.clearScreen
    val player2: Player = game.player1.copy(_isTurnToPlay = false)
    game.copy(_player1 = player1, _player2 = player2)
  }


  @tailrec
  def askAI2ToShoot(game: Game): Game = {
    //here the player1 in the game is the AI2
    val r = game.player1.random.get
    val x = r.nextInt(Grid.SIZE)
    val y = r.nextInt(Grid.SIZE)

    println("AI 3 attaque")
    println("x: "+x+ "; y =" + y)
    println("Cell: "+ Cell(x,y))
    haveBeenTargeted(game.player2.gridStates, Cell(x, y)) match {
      //if it was not yet targeted
      case false =>{
        val cell: Cell = Cell(x,y)
        println("IA 2 : " + cell)
        val player1: Player = Utility.shoot(game.player2, cell.x, cell.y)
        val player2: Player = game.player1.copy(_isTurnToPlay = false, _random = Some(r))
        game.copy(_player1 = player1, _player2 = player2)
      }

      //if it has been targeted
      case _ => {
        this.askAI2ToShoot(game)
      }
    }
  }

  @tailrec
  def askAI3ToShoot(game: Game): Game = {
    //player1 is considered as AI3
    if (game.player1.targeted.isEmpty) {
      val x = game.player1.random.get.nextInt(Grid.SIZE)
      val y = game.player1.random.get.nextInt(Grid.SIZE)
      if (Utility.haveBeenTargeted(game.player2.gridStates, Cell(x, y))) {
        askAI3ToShoot(game.copy())
      }
      else {
        val toTarget: List[Cell] = Utility.filterValidCells(generatePotentialCells(x, y), Nil)
        askAI3ToShoot(game.copy(_player1 = game.player1.copy(_targeted = toTarget)))
      }
    }
    else {
      Utility.haveBeenTargeted(game.player2.gridStates, game.player1.targeted.head) match {
        case true => {
          askAI3ToShoot(game.copy(_player1 = game.player1.copy(_targeted = game.player1.targeted.tail)))
        }
        case _ => {
          val player1 = shoot(game.player2, game.player1.targeted.head.x, game.player1.targeted.head.y)
          player1.gridStates.gridStates(game.player1.targeted.head.y)(game.player1.targeted.head.x) match {
            case Utility.HIT_STATUS => {
              game.copy(_player1 = player1, _player2 = game.player1.copy(_isTurnToPlay = false, _targeted = game.player1.targeted.tail))//:::Utility.filterValidCells(generatePotentialCells(game.player1.targeted.head.x, game.player1.targeted.head.y),Nil)))
            }
            case _ => {
              game.copy(_player1 = player1, _player2 = game.player1.copy(_isTurnToPlay = false, _targeted = game.player1.targeted.tail))
            }
          }
        }
      }
    }
  }




  def generatePotentialCells(x: Int, y: Int): List[Cell] = {
    Cell(x-1,y)::Cell(x+1,y)::Cell(x,y-1)::Cell(x,y+1)::Nil
  }

  @tailrec
  def filterValidCells(potential: List[Cell], valid: List[Cell]): List [Cell] = {
    if (potential.isEmpty){
      valid
    }
    else if (Cell.isValid(potential.head.x, potential.head.y)){
          filterValidCells(potential.tail, potential.head::valid)
        }
        else {
         filterValidCells(potential.tail, valid)
        }
  }





/**
  * si ai.testedDirection est vide:
  *   check si nouvelle celllule générée a déja été testée ou non
  *   si oui : on lui redemande un autre
  *   si non:
  */



  //says whether or not a square have been targeted
  def haveBeenTargeted(opponentGrid: Grid, cell: Cell): Boolean = {
    println("$$$$---==== "+cell)
    println(opponentGrid.gridStates(cell.y)(cell.x))
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
