import game._
import iohandler._
import player._

import scala.util.Random

object Main extends App {


  gameBuilder()

  def gameBuilder(): Unit =
    Utility.askPlayerMode() match {
      case "Human mode" => {
        val player1Name: String = Utility.askForName("first")
        val player2Name: String = Utility.askForName("second")
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil), Nil, None, None, None)
        val player2: Player = Player(Nil, player2Name, None, 0, _isTurnToPlay = false, Grid(Nil), Nil, None, None, None)
        //ask user for number of round
        Display.printAskForNumberOfRound
        val nbRound: Int = Input.getNumberOfRound.toInt
        val score = mainLoop(None, player1, player2, nbRound)
        Display.printEndOfGame(score)
      }
      case "AI mode" => {
        val r1 = new Random
        val r2 = new Random
        val r3 = new Random
        val ai1: Player = Player(Nil, "AI Level Beginner", Some(1), 0, _isTurnToPlay =  false, Grid(Nil), Nil, None, Some(r1), None)
        val ai2: Player = Player(Nil, "AI Level Medium", Some(2), 0, _isTurnToPlay =  false, Grid(Nil), Nil, None, Some(r2), None)
        val ai3: Player = Player(Nil, "AI Level Hard", Some(3), 0, _isTurnToPlay =  false, Grid(Nil), Nil, None, Some(r3), None)

        //A1 vs AI2
        val records1: String = mainLoop(None, ai1.copy(_isTurnToPlay = true), ai2, Utility.NUMBER_OF_TOTAL_ROUND)

        //AI1 vs AI3
        val records2: String = mainLoop(None, ai1.copy(_isTurnToPlay = true), ai3, Utility.NUMBER_OF_TOTAL_ROUND)

        val test: String = records1+records2
        Output.writeResult(test)

        //AI2 vs AI3
        val records3: String = mainLoop(None, ai2.copy(_isTurnToPlay = true), ai3, Utility.NUMBER_OF_TOTAL_ROUND )
        //Output.writeResult(records3)
        //val ai_proof: String = records1+records2+records3

        //Output.writeResult(ai_proof)
      }
      case "AI vs AI" => {
        val r1 = new Random
        val r2 = new Random
        val r3 = new Random
        val ai1: Player = Player(Nil, "AI Level Beginner", Some(1), 0, _isTurnToPlay =  false, Grid(Nil), Nil, None, Some(r1), None)
        val ai2: Player = Player(Nil, "AI Level Medium", Some(2), 0, _isTurnToPlay =  false, Grid(Nil), Nil, None, Some(r2), None)
        val ai3: Player = Player(Nil, "AI Level Hard", Some(3), 0, _isTurnToPlay =  false, Grid(Nil), Nil, None, Some(r3), None)

        //A1 vs AI2
       /* val records1: String = mainLoop(None, ai1.copy(_isTurnToPlay = true), ai2, Utility.NUMBER_OF_TOTAL_ROUND)

        //AI1 vs AI3
        val records2: String = mainLoop(None, ai1.copy(_isTurnToPlay = true), ai3, Utility.NUMBER_OF_TOTAL_ROUND)

        val test: String = records1+records2
        //Output.writeResult(test)*/

        //AI2 vs AI3
        val records3: String = mainLoop(None, ai2.copy(_isTurnToPlay = true), ai3, Utility.NUMBER_OF_TOTAL_ROUND )

        //val ai_proof: String = records1+records2+records3

        //Output.writeResult(ai_proof)
      }

      case _ => Display.printSomethingWentWrong
    }



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