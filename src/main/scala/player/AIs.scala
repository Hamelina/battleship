package player

import config.Config
import game.{Game, Utility}
import game.Utility.{haveBeenTargeted, shoot}
import iohandler.Display
import ship.{Cell, Ship}

import scala.annotation.tailrec
import scala.util.Random

object AIs {
  /**
    * Ask an AI to place ships.
    *
    * @param shipFormat A list of tuple that corresponds to a list of number and size of ship that needs to be placed on the grid so that the game can begin.
    * @param fleet The current fleet of the Player.
    * @param player The player who is asked to place his/her/its ships.
    * @return A list of ships that corresponds to the coordinates of the ship the the player.
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
      val x = r.nextInt(Config.SIZE)
      val y = r.nextInt(Config.SIZE)
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
    * Ask the AI1 involved in the game to shoot.
    * @param game The current game for which the AI1 is asked to shoot.
    * @return A new game that corresponds to the game given in argument but after the AI1 has shoot.
    */
  def askAI1ToShoot(game: Game): Game = {
    val cell: Cell = Cell(game.player1.random.get.nextInt(Config.SIZE
    ), game.player1.random.get.nextInt(Config.SIZE
    ))
    val player1: Player = Utility.shoot(game.player2, cell.x, cell.y)
    Display.clearScreen
    val player2: Player = game.player1.copy(_isTurnToPlay = false)
    game.copy(_player1 = player1, _player2 = player2)
  }

  /**
    * Ask the AI2 involved in the game to shoot.
    * @param game The current game for which the AI2 is asked to shoot.
    * @return A new game that corresponds to the game given in argument but after the AI2 has shoot.
    */
  @tailrec
  def askAI2ToShoot(game: Game): Game = {
    //here the player1 in the game is the AI2
    val r = game.player1.random.get
    val x = r.nextInt(Config.SIZE
    )
    val y = r.nextInt(Config.SIZE
    )

    haveBeenTargeted(game.player2.gridStates, Cell(x, y)) match {
      //if it was not yet targeted
      case false =>{
        val cell: Cell = Cell(x,y)
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

  /**
    * Ask the AI3 involved in the game to shoot.
    * @param game The current game for which the AI3 is asked to shoot.
    * @return A new game that corresponds to the game given in argument but after the AI3 has shoot.
    */
  @tailrec
  def askAI3ToShoot(game: Game): Game = {
    //player1 is considered as AI3
    if (game.player1.toTarget.isEmpty) {
      val x = game.player1.random.get.nextInt(Config.SIZE
      )
      val y = game.player1.random.get.nextInt(Config.SIZE
      )
      if (Utility.haveBeenTargeted(game.player2.gridStates, Cell(x, y))) {
        askAI3ToShoot(game.copy())
      }
      else {
        val toTarget: List[Cell] = AIs.filterValidCells(generatePotentialCells(x, y), Nil)
        askAI3ToShoot(game.copy(_player1 = game.player1.copy(_toTarget = toTarget)))
      }
    }
    else {
      Utility.isHit(game.player2.fleet, game.player1.toTarget.head) match {
        case true => {
          askAI3ToShoot(game.copy(_player1 = game.player1.copy(_toTarget = game.player1.toTarget.tail)))
        }
        case _ => {
          val player1 = shoot(game.player2, game.player1.toTarget.head.x, game.player1.toTarget.head.y)
          player1.gridStates.gridStates(game.player1.toTarget.head.y)(game.player1.toTarget.head.x) match {
            case Utility.HIT_STATUS => {
              game.copy(_player1 = player1, _player2 = game.player1.copy(_isTurnToPlay = false, _toTarget = game.player1.toTarget.tail:::AIs.filterValidCells(generatePotentialCells(game.player1.toTarget.head.x, game.player1.toTarget.head.y),Nil)))
            }
            case _ => {
              game.copy(_player1 = player1, _player2 = game.player1.copy(_isTurnToPlay = false, _toTarget = game.player1.toTarget.tail))
            }
          }
        }
      }
    }
  }


  /**
    * Generates a list of potential cells that can be hit around a cell
    * @param x The coordinates on x-axis of the concerned cell.
    * @param y The coordinates on y-axis of the concerned cell.
    * @return A list of cells whose parameters have been given as arguments of the function.
    */
  def generatePotentialCells(x: Int, y: Int): List[Cell] = {
    Cell(x-1,y)::Cell(x+1,y)::Cell(x,y-1)::Cell(x,y+1)::Nil
  }


  /**
    * Filters a list of potential cells and returns the valid ones.
    * @param potential The list of cells to filter.
    * @param valid  The valid cells.
    * @return A list of a valid cells given a list of cells.
    */
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




}
