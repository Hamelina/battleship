package iohandler
import game._
import scala.annotation.tailrec
/**
  * An object in charge of all the display in the battleship game.
  */
object Display{

  /**
    * Prints a welcome message
    */
  def printWelcome: Unit = {
    print("\n Welcome to you ! You have started the most enjoyable game ever: the battleship !")
  }

  /**
    * Prints a message to ask for a number of round.
    */
  def printAskForNumberOfRound: Unit = {
    print("\n How many round do you want to play ? \n" )
  }

  /**
    * Prints a message to tell a player it is his/her turn to play.
    * @param playerName The name of the player who is up to play.
    */
  def printAskForTarget(playerName: String): Unit ={
    print(s"\n $playerName it is your turn to play. Please enter the targeted square in the following format: x y \n")
  }

  /**
    *Clears the screen.
    */
  def clearScreen: Unit = {print("\\033[H\\033[2J")}


  /**
    * Prints a message to announce the player's grid.
    */
  def printAnnounceMyGrid: Unit ={
    print("\n Here is your grid: \n")
  }

  /**
    * Prints a message to announce the opponent's grid.
    */
  def printAnnounceOpponentGrid: Unit ={
    print("\n Here is your opponent's grid: \n")
  }

  /**
    * Prints a message telling the format is not right.
    */
  def printFormatNotValid: Unit = {
    print("\n The format is not right, please try again ! \n")
  }

  /**
    * Prints a message telling the position of the starting point is not valid.
    */
  def printStartPositionNotValid: Unit = {
    print("\n These coordinates are not valid, either the coordinates do not fit into the grid or a ship is already on the involved square(s). Please try again: \n")
  }

  /**
    * Prints a message telling the coordinates are not valid.
    */
  def printCoordinatesNotValid: Unit = {
    print("\n The coordinates are not valid. Please try again: \n")

  }

  /**
    * Prints the coordinates are incorrect.
    */
  def printCoodinatesNotCorrect: Unit = {
    print("\n Your input are incorrect, please try again: \n")
  }

  /**
    * Prints a message telling the user number "order" to enter his/her name
    * @param order A string that corresponds either to "first" or "second", meaning the first or the second player.
    */
  def printAskName(order: String): Unit = {
    print(s"\n Please enter the name of the $order player: \n")
  }

  /**
    *Prints a message telling the user to type the level of AI he/she wants to challenge.
    */
  def printAskLevel: Unit = {
    print("\n Please enter the level of the AI you want to challenge : \n")
    print("\n   1. You against AI Level Beginner")
    print("\n   2. You against AI Level Medium")
    print("\n   3. You against AI Level Hard \n")
  }

  /**
    * Prints a message telling the user to type the game mode he/she want to player with.
    */
  def printAskMode: Unit = {
    print("\n On which mode do you want to play ? Please press the number corresponding to it.")
    print("\n   1. You against an AI")
    print("\n   2. You against an Humain")
    print("\n   3. Ais battleship \n")
  }

  /**
    * Prints a message telling the user the input corresponding to the level is incorrect.
    */
  def printLevelIncorrect: Unit = {
    print("\n Incorrect input, it should be either '1' or '2' or '3'. Please try again \n")
  }

  /**
    * Prints a message telling the user the input corresponding to the game mode is incorrect.
    */
  def printModeIncorrect: Unit = {
    print("\n Incorrect input, it should be either '1' for '2' or '3'. Please try again \n")
  }

  /**
    * Prints a message asking the user to type the starting point of a ship, given a format.
    * @param size Int corresponding to the size of the ship the user have to give the coordinates of the starting point of.
    */
  def printAskStartingPoint(size: Int): Unit = {
    print(s"\n Please enter the starting point on x-axis (0-0), y-axis(0-9) and the direction (U: up, D: down, R: right, L:left) of the ship of size $size , like so: x y direction : \n")
  }

  /**
    * Print a message telling the user that coordinates of the ship are not valid.
    */
  def printShipNotValid: Unit = {
    print("\n The ship does not fit to the grid")
  }

  /**
    * Prints a message telling the user the last input was wrong.
    */
  def printWrongInput: Unit = {
    print("\n The input is wrong!")
  }

  /**
    * Prints a message notifying the user the game is finished.
    * @param result String that corresponds to the final score.
    */
  def printEndOfGame(result: String): Unit = {
    print(s"s\n ****Game finished**** \n $result")
  }


  /**
    * Prints the grid of the player who is currently up to play.
    * @param game The current game.
    */
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
    * Prints the grid of the opponent's (in the point of view of the player who is currently up to play).
    * @param game The current game.
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

  /**
    * Print a separator (used here to separate 2 grids).
    */
  def printSeparator: Unit = { print("\n -------------")}


  /**
    * Prints a message asking a user to enter a ship of a given size.
    * @param playerName The name of the user who is asked.
    * @param size The size to the ship which is needed to be asked to the user.
    */
  def printAskShipToUser(playerName: String, size: Int): Unit = { print(s"\n $playerName please enter the starting point on x-axis (0-0), y-axis(0-9) and the direction (U: up, D: down, R: right, L:left) of the ship of size $size , like so: x y direction => ")
  }

  /**
    * Prints a message notifying the user a ship was sunk.
    */
  def printShipSunk: Unit = {
    print("\n Niiiice, a ship was sunk ! Very good !")
  }

  /**
    * Prints a message notifying the user a square was hit.
    */
  def printTargetHit: Unit = {
    print("\n You HIIIIIIIIIIT it! Well played!")
  }

  /**
    * Prints a message notifying the user a shoot was missed.
    */
  def printTargetMissed: Unit = {
    print("\n You MISSED it :'( keep trying ... ")
  }

  /**
    * Prints a message notifying the user the game is over.
    */
  def printGameOver: Unit = println("\n=== GAME OVER ===")


  /**
    * Prints a message notifying the user(s) who won the current round.
    * @param playerName The name of the winner.
    */
  def printVictory(playerName: String): Unit = {
    print(s"\n Congratulations to $playerName , you won this round ! ")
  }


  /**
    * Prints a message giving the current score.
    * @param winnerName The name of the one who won the last round.
    * @param winnerScore The score of the one who won the last round.
    * @param looserName The name of the one who lost the last round.
    * @param looserScore The score of the one who lost the last round.
    */
  def printScore(winnerName: String, winnerScore: Int, looserName: String, looserScore: Int): Unit = {
    print(s"\n Score: $winnerName : $winnerScore VS $looserName : $looserScore ")
  }

  /**
    * Prints a message notifying the user(s) who won the current round.
    * @param winnerName The name of the winner.
    */
  def printWinner(winnerName: String): Unit = {
    print(s"\n Congratulations to $winnerName, you won this round ! ")
  }

  /**
    * Prints a message telling something went wrong the can not identify what it is.
    */
  def printSomethingWentWrong: Unit = {
    print("\n Something went wrong. The source code must have been changed by someone")
  }

  /**
    * Prints the end of the game.
    */
  def printEndOfGame: Unit = {
    print("\n ================ END OF GAME ================ \n")
  }
}
