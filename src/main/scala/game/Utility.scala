package game

import iohandler._
import player.Player
import ship.{Cell, Ship}

import scala.annotation.tailrec
import scala.util.Random

/**
  * This class is an utility in which the settings of the game are stored and it provides functions to display.
  */
case class Utility(private val _game: Game) {
  def game: Game = this._game
}

object Utility {
  val GRID_LIMIT_X = 9
  val GRID_LIMIT_Y= 9
  val BLOCK = "██"
  val OCCUPIED_STATUS = "occupied"
  val MISSED_STATUS = "missed"
  val HIT_STATUS = "hit"
  val NOT_TARGETED = "not targeted"

  //Size and number of the different ships allowed during the game
  //TODO uncomment the line below before the deadline
  //val NUMBER_AND_SIZE_OF_SHIPS: List[List[Int]] = List(List(1,1),List(1,2),List(2,3),List(1,4),List(1,5))
  val NUMBER_AND_SIZE_OF_SHIPS: List[List[Int]] = List(List(1,1),List(1,2))
  val NUMBER_OF_TOTAL_ROUND: Int = 100
  val NB_SHIP = 6
  val CARRIER: (Int, Int) = (1, 1)
  val BATTLESHIP: (Int, Int) = (2, 1)
  val CRUISER: (Int, Int) = (3, 1)
  val SUBMARINE: (Int, Int) = (4, 1)
  val DESTROYER: (Int, Int) = (5, 1)
  //

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






  @tailrec
  def askAIForShipSetting(shipFormat: List[List[Int]], fleet: List[List[Cell]], random: scala.util.Random): List[Ship] = {
    //there is no ship left to place
    if (shipFormat.isEmpty) {
      fleet.map(x => Ship(x.size, x))
    }
    else if (1 > shipFormat.head(0)){
      askAIForShipSetting(shipFormat.tail, fleet, random)
    }
    //there is still at least one ship left to place
    else
    {
      //we arbitrary set the direction to R
      val x = random.nextInt(Grid.SIZE)
        val y = random.nextInt(Grid.SIZE)
      val l2 = Ship.createCellsList(size = shipFormat.head(1), direction = "R", Cell(x, y) :: Nil)
      val isValidShip: Boolean = Ship.isValid(x = x, y = y, shipFormat.head(1), direction = "R")

      //if the settings generated for the ship to create are not superposed to the the settings of the existing fleet
      if (!isValidShip) {
        askAIForShipSetting(shipFormat, fleet = fleet, random)
      }
      //if the inputs are valid
      else if (isValidShip && !Ship.hasAtLeastOneElement(l1 = fleet, l2))  {
        askAIForShipSetting(shipFormat.updated(0, shipFormat.apply(0).updated(0, shipFormat.head(0)-1)),fleet = l2+:fleet, random)
      }
      else {
        askAIForShipSetting(shipFormat, fleet, random)
      }
    }
  }







  //a function that places ships on a grid for 2 players, is returns a game.
  /**
    * This function asks a user to enter some information
    * @param size The maximum size of the ship
    * @param fleet The list of initial existing occupied cells corresponding to the initial existing fleet
    * @return A list of ships that corresponds the possible ships created by the input of the user
    */
  @tailrec
  def askUserForShipSettings(shipFormat: List[List[Int]], fleet: List[List[Cell]], playerName: String): List[Ship] = {

    //there is no ship left to place
    if (shipFormat.isEmpty) {
      fleet.map(x => Ship(x.size, x))
    }

      //there is still ship left to place and its number is 0
    else if (1 > shipFormat.head(0)){
      askUserForShipSettings(shipFormat.tail, fleet, playerName)
    }
    //there is still at least one ship left to place
    else
    {
      Display.printAskShipToUser(playerName, shipFormat.head(1))
      val settings: String = Input.startingPoint
      val startingPoint: Array[String] = settings.split(" ")
      startingPoint.size match {
        case 3 => {

          val l2 = Ship.createCellsList(size = shipFormat.head(1), direction = startingPoint(2), Cell(startingPoint(0).toInt, startingPoint(1).toInt) :: Nil)
          val isValidShip: Boolean = Ship.isValid(x = startingPoint(0).toInt, y = startingPoint(1).toInt, shipFormat.head(1), direction = startingPoint(2))

          //if the settings entered for the ship to create are not superposed to the the settings of the existing fleet
          if (!isValidShip) {
            Display.printStartPositionNotValid
            askUserForShipSettings(shipFormat, fleet = fleet, playerName)
          }
          //if the inputs are valid
          else if (isValidShip && !Ship.hasAtLeastOneElement(l1 = fleet, l2 = l2))  {
            askUserForShipSettings(shipFormat.updated(0, shipFormat.apply(0).updated(0, shipFormat.head(0)-1)),fleet = l2+:fleet, playerName)
          }
          else {
            Display.printCoordinatesNotValid
            askUserForShipSettings(shipFormat, fleet = fleet, playerName)
          }
        }
        //the input is identified as a quit
        case _ => {
          Display.printCoodinatesNotCorrect
          askUserForShipSettings(shipFormat, fleet, playerName)
        }
      }
    }
  }


  /**
    * This function asks a user to enter some information
    * @param size The size of the ship
    */
  /*def askShipsOfSize(size: Int): Option[Ship] = {
    Display.printAskStartingPoint(size)
    val settings = Input.startingPoint
    val startingPoint = settings.split(" ")
      startingPoint.size match{
        case 3 => {
          //if the inputs are valid
          if (Ship.isValid(x = startingPoint(0).toInt, y = startingPoint(1).toInt, size, direction = startingPoint(2))) {
            val l2 = Ship.createShipFromStartingPoint(Cell(startingPoint(0).toInt, startingPoint(1).toInt), size, startingPoint(2))
            println(l2)
            Some(l2)
          }
          else {
            Display.printShipNotValid
            None
          }

        }
        //the input is identified as a quit
        case _ => {
          Display.printWrongInput
          None
        }
      }
    }*/




  //1st player returned --> player, second--> opponent

  /**
    * This function return whether a ship is sunk or not
    * @param fleet
    * @param cell
    * @return
    */
  def isShipSunk(fleet: List[Ship], cell: Cell): Boolean = {
    if (fleet.isEmpty)  false
    else if (fleet.head.cells.contains(cell)) true
    else isShipSunk(fleet.tail, cell)
  }

  def initializeRound(looser: Player, winner: Player, random: Random): Game = {
    val listShipPlayer1: List[Ship] = Utility.askForShipSettings(NUMBER_AND_SIZE_OF_SHIPS, Nil, looser.name, looser.level, random)
    val gridPlayer1: Grid = Grid.initializeGridFromFleet(listShipPlayer1, Grid.SIZE)
    val player1: Player = looser.copy(_fleet = listShipPlayer1, _gridStates = gridPlayer1, _isTurnToPlay = true)


    val listShipPlayer2: List[Ship] = Utility.askForShipSettings(Utility.NUMBER_AND_SIZE_OF_SHIPS, Nil, winner.name, winner.level, random)
    val gridPlayer2: Grid = Grid.initializeGridFromFleet(listShipPlayer2, Grid.SIZE)
    val player2: Player = winner.copy(_fleet = listShipPlayer2, _gridStates = gridPlayer2)

    Game(player1, player2, random)
  }

  def askPlayerMode(): String = {
    Display.printAskMode
    val mode = Input.mode
    mode match {
      case "AI" => "AI mode"
      case "H" => "Human mode"
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

  def askForShipSettings(shipFormat: List[List[Int]], fleet: List[List[Cell]], playerName: String, playerLevel: Option[Int], random: Random): List[Ship] = {
    playerLevel match{
      case None => Utility.askUserForShipSettings(Utility.NUMBER_AND_SIZE_OF_SHIPS, Nil, playerName)
      case _ => Utility.askAIForShipSetting(Utility.NUMBER_AND_SIZE_OF_SHIPS, Nil, random)
    }
  }

  def askForName(order: String): String = {
    Display.printAskName(order)
    Input.name
  }

  def askPlayerToShoot(game: Game): Game ={
    Display.printAskForTarget(game.player1.name)
    if (game.player1.isTurnToPlay) {
      game.player1.level match {
        case  None => {
          Display.clearScreen
          Display.printAnnounceMyGrid
          Display.printMyGrid(game)
          Display.printSeparator
          Display.printAnnounceOpponentGrid
          Display.printOpponentGrid(game)
          val cell: Cell = Utility.askUserToShoot(game.player1.name)
          val player1: Player = Utility.shoot(game.player2, cell.x, cell.y)
          Display.clearScreen
          val player2: Player = game.player1.copy(_isTurnToPlay = false)
          game.copy(_player1 = player1, _player2 = player2)
        }

        //TODO for the  AI
        //level1
        case Some(1) => askAI1ToShoot(game.copy())
        //level2
        case Some(2) => askAI2ToShoot(game.copy())
        //level3
        case _ => askAI3ToShoot(game.copy())

      }
    }
    else {
      //it ut to the player2 to shoot
      game.player2 level match {
        case  None => {
          Display.clearScreen
          Display.printAnnounceMyGrid
          Display.printMyGrid(game)
          Display.printSeparator
          Display.printAnnounceOpponentGrid
          Display.printOpponentGrid(game)
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
        case Some(2) => askAI2ToShoot(game.copy(_player1 = game.player2, _player2 = game.player1))
        //level3
        case _ => askAI3ToShoot(game.copy(_player1 = game.player2, _player2 = game.player1))

      }
    }


  }



  //TODO continu for the AI
  def askAI1ToShoot(game: Game): Game = {
    val cell: Cell = Cell(game.random.nextInt(Grid.SIZE), game.random.nextInt(Grid.SIZE))
    val player1: Player = Utility.shoot(game.player2, cell.x, cell.y)
    Display.clearScreen
    val player2: Player = game.player1.copy(_isTurnToPlay = false)
    game.copy(_player1 = player1, _player2 = player2)
  }


  def askAI2ToShoot(game: Game): Game = {
  //here the player1 in the game is the AI2
      val x = game.random.nextInt(Grid.SIZE)
      val y = game.random.nextInt(Grid.SIZE)
      game.player2.gridStates.gridStates(x)(y) match {
        //if it was not yet targeted
        case Utility.NOT_TARGETED =>{
          val cell: Cell = Cell(x,y)
          val player1: Player = Utility.shoot(game.player2, cell.x, cell.y)
          Display.clearScreen
          val player2: Player = game.player1.copy(_isTurnToPlay = false)
          game.copy(_player1 = player1, _player2 = player2, _random = game.random)
        }

        //if it has been targeted
        case _ => {
          this.askAI2ToShoot(game.copy(_random = game.random))
        }
      }
    }

  @tailrec
  def askAI3ToShoot(game: Game): Game = {
    //player1 is considered as AI3
    val ai: Player = game.player1

    ai.testedDirection.size match {

        //nothing was hit before
      case 0 =>{
        val x = game.random.nextInt(Grid.SIZE)
        val y = game.random.nextInt(Grid.SIZE)
        val cellToTarget = Cell(x, y)

        //if the square have already been targeted or if the player have already hit this square
        if (haveBeenTargeted(game.player2.gridStates, cellToTarget) || game.player1.hit.contains(cellToTarget)){
          askAI3ToShoot(game.copy(_random = game.random))
        }
        else{

          val player1: Player = Utility.shoot(game.player2, cellToTarget.x, cellToTarget.y)

          //see what it was: a hit or miss
          val newGame: Game = player1.gridStates.gridStates(cellToTarget.x)(cellToTarget.y) match {

            //if hit
            case Utility.HIT_STATUS => {

              //if the square is in the last column
              if (cellToTarget.x==Grid.SIZE-1){

                //nextime check directly to the left
                  game.copy(_player1 = player1, _player2 = ai.copy(_hit = cellToTarget :: ai.hit, _testedDirection = List("Right", "Left"), _isTurnToPlay = false), _random = game.random)


                }
              else{
                //explore the right side of the targeted
                game.copy(_player1 = player1, _player2 = ai.copy(_hit = cellToTarget::ai.hit, _testedDirection = List("Right"), _isTurnToPlay = false), _random = game.random)
              }
            }
            //so ship founded
            case _ => {
              //square missed, choose randomly next time
              game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = Nil), _random = game.random)
            }
          }
          newGame
        }
      }
        //check si la case n'est pas déja contenue dans la liste des hit


        //the last action was a hit, so first see if there is nothing left in its right
      case 1 => {
        val cellToTarget: Cell = Cell(ai.hit.head.x+1, ai.hit.head.y)
        //the square on the right of the target have already been tested so --> ask with with direction left testedDirection
        if (haveBeenTargeted(game.player2.gridStates, cellToTarget)){
          askAI3ToShoot(game.copy(_player1 = ai.copy(_testedDirection = List("Right", "Left"))))
        }
        else {
          //shoot to that cellTargets
          val player1: Player = Utility.shoot(game.player2, cellToTarget.x, cellToTarget.y)

          //according to the result, change the state of the player
          val newGame: Game = player1.gridStates.gridStates(cellToTarget.x)(cellToTarget.y) match {

            case Utility.HIT_STATUS =>{

              if(cellToTarget.x==Grid.SIZE-1){
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = List("Right", "Left"), _hit = cellToTarget::ai.hit), _random = game.random)
              }
              else{
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = List("Right"), _hit = cellToTarget::ai.hit), _random = game.random)
              }
            }
            case _ => {

              //cannot turn on the left anymore
              if(cellToTarget.x==0){

                //next time target directly up
                if (cellToTarget.y==0){
                  //check directly down the square
                  game.copy(_player1 = player1, _player2 = ai.copy(_hit = cellToTarget :: ai.hit, _testedDirection = List("Right","Left", "Up", "Down"), _isTurnToPlay = false), _random = game.random)
                }

                  //goes directly down
                else {
                  game.copy(_player1 = player1, _player2 = ai.copy(_testedDirection = List("Right","Left", "Up"), _isTurnToPlay = false), _random = game.random)
                }

              }
              else{
                //next time turn to the right again
                game.copy(_player1 = player1, _player2 = ai.copy(_testedDirection = List("Right","Left"), _isTurnToPlay = false), _random = game.random)

              }
            }
          }
          newGame
        }
          //if contains: on rajoute à la tête de la tête de hit cellToTarget :: ai.hit et reappeler la fonction
      }

      //a hit has been made and the squares on the right side have been tested already, so see if there is nothing left in its left
      case 2 =>{
        val cellToTarget: Cell = Cell(ai.hit.head.x-1, ai.hit.head.y)

        //if the square on the left of the target have already been tested so --> ask with an empty testedDirection
        if (haveBeenTargeted(game.player2.gridStates, cellToTarget)){
          askAI3ToShoot(game.copy(_player1 = ai.copy(_testedDirection = List("Right", "Left", "Up"))))
        }
        else {
          //shoot to that cellTargets
          val player1: Player = Utility.shoot(game.player2, cellToTarget.x, cellToTarget.y)

          //according to the result, change the state of the player
          val newGame: Game = player1.gridStates.gridStates(cellToTarget.x)(cellToTarget.y) match {

              //the ship is horizontal
            case Utility.HIT_STATUS =>{
              //arrivé au bout
              //il demande si la case de droit est contenu dans les hit --> c'est un bateau horizontal donc next step --> on vide targetedDirection


              //if it is on the first column then since it is horizontal, it is not neccessary to check up
              if(cellToTarget.x==0){
                //no need to check up because the ship is horizontal
                  game.copy(_player1 = player1, _player2 = ai.copy(_hit = cellToTarget :: ai.hit, _testedDirection = Nil, _isTurnToPlay = false), _random = game.random)
                }

                //continue to check left
              else {
                  game.copy(_player1 = player1, _player2 = ai.copy(_hit = cellToTarget :: ai.hit, _testedDirection = List("Right","Left"), _isTurnToPlay = false), _random = game.random)
                }
            }

            case _ => {

              //the last targeted cell is in the last column
              if (cellToTarget.x-1==Grid.SIZE-1){
                if (cellToTarget.y==0){
                  game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = List("Right", "Left", "Up", "Down")), _random = game.random)
                }
                else{
                  game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = List("Right", "Left", "Up")), _random = game.random)
                }
              }
                //it means she ship is horizontal so no need to check up or down again
              else {
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = Nil), _random = game.random)
              }
            }
          }
          newGame
        }
      }

      //a hit has been made and the squares on the right and left sides have been tested already, so see if there is nothing left up the concerned square
      case 3 =>{
        val cellToTarget: Cell = Cell(ai.hit.head.x, ai.hit.head.y-1)

        //if the square on the left of the target have already been tested so --> ask with an empty testedDirection
        if (haveBeenTargeted(game.player2.gridStates, cellToTarget)){
          askAI3ToShoot(game.copy(_player1 = ai.copy(_testedDirection = List("Right", "Left", "Up", "Down"))))
        }
        else {
          //shoot to that cellTargets
          val player1: Player = Utility.shoot(game.player2, cellToTarget.x, cellToTarget.y)

          //according to the result, change the state of the player
          val newGame: Game = player1.gridStates.gridStates(cellToTarget.x)(cellToTarget.y) match {
            //the ship is horizontal
            case Utility.HIT_STATUS =>{
              if (cellToTarget.y==0){
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _hit= cellToTarget::ai.hit, _testedDirection = List("Right", "Left", "Up", "Down")), _random = game.random)
              }
              else{
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _hit= cellToTarget::ai.hit, _testedDirection = List("Right", "Left", "Up", "Down")), _random = game.random)

              }
            }
            case _ => {
              //if cellToTarget is in the last row
              if (cellToTarget.y==Grid.SIZE-1){
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = Nil), _random = game.random)
              }
              else{
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = List("Right", "Left", "Up", "Down")), _random = game.random)
              }
            }
          }
          newGame
        }
      }

      //a hit has been made and the squares on the right and left sides have been tested already, so see if there is nothing left down the concerned square
      case 4 =>{
        val cellToTarget: Cell = Cell(ai.hit.head.x, ai.hit.head.y+1)

        //if the square on the left of the target have already been tested so --> ask with an empty testedDirection
        if (haveBeenTargeted(game.player2.gridStates, cellToTarget)){
          askAI3ToShoot(game.copy(_player1 = ai.copy(_testedDirection = Nil)))
        }
        else {
          //shoot to that cellTargets
          val player1: Player = Utility.shoot(game.player2, cellToTarget.x, cellToTarget.y)

          //according to the result, change the state of the player
          val newGame: Game = player1.gridStates.gridStates(cellToTarget.x)(cellToTarget.y) match {

            //the ship is horizontal
            case Utility.HIT_STATUS =>{
              if (cellToTarget.y==Grid.SIZE-1){
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _hit= cellToTarget::ai.hit, _testedDirection = Nil), _random = game.random)
              }
              else{
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _hit= cellToTarget::ai.hit, _testedDirection = List("Right", "Left", "Up", "Down")), _random = game.random)

              }
            }
            case _ => {
                game.copy(_player1= player1, _player2 = ai.copy(_isTurnToPlay = false, _testedDirection = Nil), _random = game.random)
              }
            }
          newGame
          }
        }

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
    opponentGrid.gridStates(cell.x)(cell.y) match {
      case Utility.NOT_TARGETED => false
      case _ => true
    }
  }

}
