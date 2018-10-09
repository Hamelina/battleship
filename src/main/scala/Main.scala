
import game._
import iohandler._
import player._
import config._

import scala.util.Random

/**
  * The main programm of the battle ship.
  */
object Main extends App {

  Display.printWelcome
  buildGame()

  /**
    * Builds the game.
    */
  def buildGame(): Unit =
    Utility.askPlayerMode() match {
      case "Human mode" => {
        val player1Name: String = Utility.askForName("first")
        val player2Name: String = Utility.askForName("second")
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil), Nil, None)
        val player2: Player = Player(Nil, player2Name, None, 0, _isTurnToPlay = false, Grid(Nil), Nil, None)
        //ask user for number of round
        Display.printAskForNumberOfRound
        val nbRound: Int = Input.getNumberOfRound.toInt
        val score = mainLoop(None, player1, player2, nbRound)
        Display.printEndOfGame(score)
      }
      case "AI mode" => {
        val player1Name: String = Utility.askForName("first")
        val AIlevel: String = Utility.askForAILevel()
        val aiName: String = "AI" + AIlevel
        val player1: Player = Player(Nil, player1Name, None, 0,  _isTurnToPlay = true, Grid(Nil), Nil, None)
        val player2: Player = Player(Nil, aiName, Some(AIlevel.toInt), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Some(new Random()))
        val score = mainLoop(None, player1, player2, Config.NUMBER_OF_TOTAL_ROUND)
        Display.printEndOfGame(score)

      }
      case "AI vs AI" => {
        val ai1: Player = Player(Nil, "AI Level Beginner", Some(1), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Some(new Random))
        val ai2: Player = Player(Nil, "AI Level Medium", Some(2), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Some(new Random))
        val ai3: Player = Player(Nil, "AI Level Hard", Some(3), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Some(new Random))

        //A1 vs AI2
        val records1: String = mainLoop(None, ai1.copy(_isTurnToPlay = true), ai2, Config.NUMBER_OF_TOTAL_ROUND)

        //AI1 vs AI3
        val records2: String = mainLoop(None, ai1.copy(_isTurnToPlay = true), ai3, Config.NUMBER_OF_TOTAL_ROUND)

        //AI2 vs AI3
        val records3: String = mainLoop(None, ai3, ai2.copy(_isTurnToPlay = true),Config.NUMBER_OF_TOTAL_ROUND )

        val ai_proof: String = records1+records2+records3

        //write the result in a csv
        Output.writeResult(ai_proof)
      }

      case _ => Display.printSomethingWentWrong
    }


  /**
    * The main loop of a game
    *
    * @param gameState An option of game that corresponds to the current gamestate: None when a player won a round.
    * @param looser If the game begins, corresponds to the player who starts the game in first. Otherwise corresponds to the player who lost the last round.
    * @param winner If the game begins, corresponds to the player who starts the game in second . Otherwise corresponds to the player who won the last round.
    * @param nbRoundsToPlay The total of round left to play.
    * @return A string that corresponds to the final score (once the number of rounds left to play is equal to 0).
    */
  def mainLoop(gameState: Option[Game], looser: Player, winner: Player, nbRoundsToPlay: Int): String = {
    Display.clearScreen
    if (nbRoundsToPlay == 0) {
      println("Game finished")
      val score: String = winner.name+";"+winner.score.toString+";"+looser.name+";"+looser.score.toString+"\n"
      score
    }
    else {

        val util: Utility = Utility(gameState.getOrElse(Utility.initializeRound(winner, looser)))

        //Ask the players to shoot
        val currentGameState: Game = Utility.askPlayerToShoot(util.game)
        //check is gameOver:
        if (currentGameState.isGameOver) {
          if (currentGameState.player1.hasLost) {
            val winner: Player = currentGameState.player2.copy(_score = currentGameState.player2.score + 1)
            Display.printVictory(winner.name)
            Display.printScore(winner.name, winner.score, currentGameState.player1.name, currentGameState.player1.score)
            mainLoop(None, currentGameState.player1, winner, nbRoundsToPlay - 1)
          }
          else {
            val winner = currentGameState.player1.copy(_score = currentGameState.player1.score + 1)
            Display.printWinner(winner.name)
            Display.printScore(winner.name, winner.score, currentGameState.player2.name, currentGameState.player2.score)
            mainLoop(None, currentGameState.player1, winner, nbRoundsToPlay - 1)
          }
        }
        else {
          mainLoop(Some(currentGameState), looser, winner, nbRoundsToPlay)
        }
    }
  }

}