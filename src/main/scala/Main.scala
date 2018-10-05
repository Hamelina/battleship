import game._
import iohandler._
import player._

import scala.util.Random

object Main extends App {


  Display.printWelcome

  gameBuilder()

  def gameBuilder(): Unit =
    Utility.askPlayerMode() match {
      case "Human mode" => {
        val player1Name: String = Utility.askForName("first")
        val player2Name: String = Utility.askForName("second")
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil), Nil, Nil)
        val player2: Player = Player(Nil, player2Name, None, 0, _isTurnToPlay = false, Grid(Nil), Nil, Nil)
        mainLoop(None, player1, player2, Game.NUMBER_OF_TOTAL_ROUND, new scala.util.Random())
      }
      case "AI mode" => {
        val player1Name: String = Utility.askForName("first")
        val aIlevel: String = Utility.askForAILevel()
        val aiName: String = "AI" + aIlevel
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil), Nil, Nil)
        val player2: Player = Player(Nil, aiName, Some(aIlevel.toInt), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Nil)
        mainLoop(None, player1, player2, Game.NUMBER_OF_TOTAL_ROUND, new scala.util.Random())
      }
        //add here the little program that runs AI vs AI
      case _ => Display.printSomethingWentWrong
    }


  def mainLoop(gameState: Option[Game], looser: Player, winner: Player, nbRoundsToPlay: Int, random: Random): Unit = {
    Display.clearScreen
    if (nbRoundsToPlay == 0) {
      println("Game finished")
      //TODO implement writeResultsIntoCsvFile function in game
      //gameState.getOrElse(Game(looser, winner).writeResultsIntoCsvFile("aiproof")
      Display.printScore(winner.name, winner.score, looser.name, looser.score)
    }
    else {

        val util: Utility = Utility(gameState.getOrElse(Utility.initializeRound(looser: Player, winner: Player, random)))

        //Ask the players to shoot
        val currentGameState: Game = Utility.askPlayerToShoot(util.game)

        //check is gameOver:
        if (currentGameState.isGameOver) {
          if (currentGameState.player1.hasLost) {
            val winner: Player = currentGameState.player1.copy(_score = currentGameState.player1.score + 1)
            Display.printVictory(winner.name)
            Display.printScore(winner.name, winner.score, currentGameState.player2.name, currentGameState.player2.score)
            mainLoop(None, currentGameState.player2, winner, nbRoundsToPlay - 1, random)
          }
          else {
            val winner = currentGameState.player2.copy(_score = currentGameState.player2.score + 1)
            Display.printWinner(winner.name)
            Display.printScore(winner.name, winner.score, currentGameState.player1.name, currentGameState.player1.score)
            mainLoop(None, currentGameState.player2, winner, nbRoundsToPlay - 1, random)
          }
        }
        else {
          mainLoop(Some(currentGameState), looser, winner, nbRoundsToPlay, random)
        }
    }
  }
}