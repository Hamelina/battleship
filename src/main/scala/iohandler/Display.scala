package iohandler

import game.Utility
import game.{Game, Grid}


import scala.annotation.tailrec

//TODO if game finished earlier, do all the print here and call them in utility.class, like so
case class Display() {

}
object Display{

  def printWelcome: Unit = {
    print("\n Welcome to you ! You have started the most enjoyable game ever: the battleship !")
  }

  def printAskForNumberOfRound: Unit = {
    print("\n How many round do you want to play ? \n" )
  }

  def printAskForTarget(playerName: String): Unit ={
    print(s"\n $playerName it is your turn to play. Please enter the targeted square in the following format: x y \n")
  }

  def clearScreen: Unit = {print("\u001b[2J")}

  def printAnnounceMyGrid: Unit ={
    print("\n Here is your grid: \n")
  }

  def printAnnounceOpponentGrid: Unit ={
    print("\n Here is your opponent's grid: \n")
  }

  def printFormatNotValid: Unit = {
    print("\n The format is not right, please try again ! \n")
  }

  def printStartPositionNotValid: Unit = {
    print("\n These coordinates are not valid, either the coordinates do not fit into the grid or a ship is already on the involved square(s). Please try again: \n")
  }

  def printCoordinatesNotValid: Unit = {
    print("\n The coordinates are not valid. Please try again: \n")

  }

  def printCoodinatesNotCorrect: Unit = {
    print("\n Your input are incorrect, please try again: \n")
  }
  def printAskName(order: String): Unit = {
    print(s"\n Please enter the name of the $order player: \n")
  }
  def printAskLevel: Unit = {
    print("\n Please enter the level of the AI you want to challenge : \n")
    print("\n   1. You against AI Level Beginner")
    print("\n   2. You against AI Level Medium")
    print("\n   3. You against AI Level Hard \n")
  }
  def printAskMode: Unit = {
    print("\n On which mode do you want to play ? Please press the number corresponding to it.")
    print("\n   1. You against an AI")
    print("\n   2. You against an Humain")
    print("\n   3. Ais battleship \n")
  }
  def printLevelIncorrect: Unit = {
    print("\n Incorrect input, it should be either '1' or '2' or '3'. Please try again \n")
  }
  def printModeIncorrect: Unit = {
    print("\n Incorrect input, it should be either 'AI' for AI or 'H' Human. Please try again \n")
  }

  def printAskStartingPoint(size: Int): Unit = {
    print(s"\n Please enter the starting point on x-axis (0-0), y-axis(0-9) and the direction (U: up, D: down, R: right, L:left) of the ship of size $size , like so: x y direction : \n")
  }
  def printShipNotValid: Unit = {
    print("\n The ship does not fit to the grid")
  }
  def printWrongInput: Unit = {
    print("\n The input is wrong!")
  }
  def printEndOfGame(result: String): Unit = {
    print(s"s\n ****Game finished**** \n $result")
  }



  /**
    * This function prints the state of the grid of the player who has to target a square at the current time
    * status: "Occupied" - "Miss" - "Not tested" - "Hit"
    * "Occupied" -> White
    * "Miss" -> Invisible
    * "Hit" -> Red
    * "Not tested" -> O Blue
    */
  //List(List("occupied","occupied","miss"), List("hit","hit","not tested")))
  def printMyGrid(game: Game): Unit = {
    print("\n  A B C D E F G H I J")
    if (game.player1.isTurnToPlay){
      //printByLine()
      stringByLine(grid2 = game.player1.gridStates.copy(_gridStates = game.player1.gridStates.copy().tail), line = game.player1.gridStates.copy().head, result = "0 " )
    }
    else{
      stringByLine(grid2 = game.player2.gridStates.copy(_gridStates = game.player2.gridStates.copy().tail), line = game.player2.gridStates.copy().head, result = "0 ")
    }

    @tailrec
    def stringByLine(grid2: Grid, line: List[String], result: String): Unit = {
      if (line.isEmpty) {
        print(s"\n $result")
        if (grid2.gridStates.nonEmpty) {
          stringByLine(grid2 = grid2.copy(_gridStates = grid2.tail), line = grid2.copy(_gridStates = grid2.gridStates).head, result= (Grid.SIZE- grid2.size).toString+" " )
        }
      }
      else stringByLine(grid2 = grid2,
        line = line.tail,
        result = result
          + (line.head match{
          case Utility.OCCUPIED_STATUS => Console.INVISIBLE+ Utility.BLOCK//"X "
          case Utility.MISSED_STATUS => Console.WHITE+ Utility.BLOCK  //"T "
          case Utility.HIT_STATUS => Console.RED+ Utility.BLOCK//"H "
          case _ => Console.BLUE+ Utility.BLOCK//"O "
        })
      )
    }
  }


  /**
    * This function prints the state of the grid of the opponent the current time
    * status: "Occupied" - "Miss" - "Not tested" - "Hit"
    * "Miss" -> Invisible
    * "Hit" -> Red
    * "Not tested" yet -> O Blue
    */
  def printOpponentGrid(game: Game): Unit ={
    println("   A B C D E F G H I J")
    if (game.player1.isTurnToPlay){
      //printByLine()
      stringByLine(grid2 = game.player2.gridStates.copy(_gridStates = game.player2.gridStates.copy().tail), line = game.player2.gridStates.copy().head, result = "0 " )
    }
    else{
      stringByLine(grid2 = game.player1.gridStates.copy(_gridStates = game.player1.gridStates.copy().tail), line = game.player1.gridStates.copy().head, result = "0 ")
    }

    @tailrec
    def stringByLine(grid2: Grid, line: List[String], result: String): Unit = {
      if (line.isEmpty) {
        print(s"\n $result")
        if (grid2.gridStates.nonEmpty) {
          stringByLine(grid2 = grid2.copy(_gridStates = grid2.tail), line = grid2.copy(_gridStates = grid2.gridStates).head, result= (Grid.SIZE-grid2.size).toString+" " )
        }
      }
      else stringByLine(grid2 = grid2,
        line = line.tail,
        result = result
          + (line.head match{
          case Utility.MISSED_STATUS => Console.WHITE+ Utility.BLOCK  //"T "
          case Utility.HIT_STATUS => Console.RED+ Utility.BLOCK //"H "
          case _ => Console.BLUE+ Utility.BLOCK //"O "
        })
      )
    }
  }


  def printSeparator: Unit = { print("\n -------------")}

  def printAskShipToUser(playerName: String, size: Int): Unit = { print(s"\n $playerName please enter the starting point on x-axis (0-0), y-axis(0-9) and the direction (U: up, D: down, R: right, L:left) of the ship of size $size , like so: x y direction => ")
  }

  def printShipSunk: Unit = {
    print("\n Niiiice, a ship was sunk ! Very good !")
  }
  def printTargetHit: Unit = {
    print("\n You HIIIIIIIIIIT it! Well played!")
  }

  def printTargetMissed: Unit = {
    print("\n You MISSED it :'( keep trying ... ")
  }

  def printGameOver: Unit = println("\n=== GAME OVER ===")

  def printVictory(playerName: String): Unit = {
    print(s"\n Congratulations to $playerName , you won this round ! ")
  }

  def printScore(winnerName: String, winnerScore: Int, looserName: String, looserScore: Int): Unit = {
    print(s"\n Score: $winnerName : $winnerScore VS $looserName : $looserScore ")
  }
  def printWinner(winnerName: String): Unit = {
    print(s"\n Congratulations to $winnerName, you won this round ! ")
  }
  def printSomethingWentWrong: Unit = {
    print("\n Something went wrong. The source code must have been changed by someone")
  }
  def printEndOfGame: Unit = {
    print("\n ================ END OF GAME ================ \n")
  }
}
