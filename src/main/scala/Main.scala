import game._
import iohandler._
import player._

import scala.util.Random

object Main extends App {

  val r = Random
  Display.printWelcome
  /*val FILENAME: String = "./ai_proof.csv"
  val record: String = "AI Level Beginner;10;AI Level Medium;100\n"
  val csvSchema = "AI Name;score;AI Name2;score2 \n"
  val bufferWriter = new BufferedWriter(new FileWriter(FILENAME))
  bufferWriter.write(csvSchema)
  bufferWriter.
  bufferWriter.write(record)
  bufferWriter.close()*/



  gameBuilder()

  def gameBuilder(): Unit =
    Utility.askPlayerMode() match {
      case "Human mode" => {
        val player1Name: String = Utility.askForName("first")
        val player2Name: String = Utility.askForName("second")
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil), Nil, Nil)
        val player2: Player = Player(Nil, player2Name, None, 0, _isTurnToPlay = false, Grid(Nil), Nil, Nil)
        //ask user for number of round
        Display.printAskForNumberOfRound
        val nbRound: Int = Input.getNumberOfRound.toInt
        val score = mainLoop(None, player1, player2, nbRound, r, 0)
        Display.printEndOfGame(score)
      }
      case "AI mode" => {
        val player1Name: String = Utility.askForName("first")
        val aIlevel: String = Utility.askForAILevel()
        val aiName: String = "AI" + aIlevel
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil), Nil, Nil)
        val player2: Player = Player(Nil, aiName, Some(aIlevel.toInt), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Nil)
        //askUser for round number
        Display.printAskForNumberOfRound
        val nbRound: Int = Input.getNumberOfRound.toInt
        val score = mainLoop(None, player1, player2, nbRound, r, 0)
        Display.printEndOfGame(score)
      }
        //add here the little program that runs AI vs AI
      case "AI vs AI" => {
        val ai1: Player = Player(Nil, "AI Level Beginner", Some(1), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Nil)
        val ai2: Player = Player(Nil, "AI Level Medium", Some(2), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Nil)
        val ai3: Player = Player(Nil, "AI Level Hard", Some(3), 0, _isTurnToPlay =  false, Grid(Nil), Nil, Nil)

        //A1 vs AI2
        val records1: String = mainLoop(None, ai1.copy(_isTurnToPlay = true), ai2, Game.NUMBER_OF_TOTAL_ROUND, r, 0)

        //AI1 vs AI3
        val records2: String = mainLoop(None, ai1.copy(_isTurnToPlay = true), ai3, Game.NUMBER_OF_TOTAL_ROUND, r,0 )

        val test: String = records1+records2
        Output.writeResult(test)

        //AI2 vs AI3
        val records3: String = mainLoop(None, ai2.copy(_isTurnToPlay = true), ai3, Game.NUMBER_OF_TOTAL_ROUND, r, 0 )

        //val ai_proof: String = records1+records2+records3

        //Output.writeResult(ai_proof)
      }

      case _ => Display.printSomethingWentWrong
    }



  def mainLoop(gameState: Option[Game], looser: Player, winner: Player, nbRoundsToPlay: Int, random: Random, nb_turn: Int): String = {
    Display.clearScreen
    if (nbRoundsToPlay == 0) {
      println("Game finished")
      //TODO implement writeResultsIntoCsvFile function in game
      val score: String = winner.name+";"+winner.score.toString+";"+looser.name+";"+looser.score.toString+"\n"
      score
    }
    else {

        val util: Utility = Utility(gameState.getOrElse(Utility.initializeRound(winner, looser , random)))

        //Ask the players to shoot
        val currentGameState: Game = Utility.askPlayerToShoot(util.game)
        println(nb_turn)
        //check is gameOver:
        if (currentGameState.isGameOver) {
          if (currentGameState.player1.hasLost) {
            val winner: Player = currentGameState.player1.copy(_score = currentGameState.player1.score + 1)
            Display.printVictory(winner.name)
            Display.printScore(winner.name, winner.score, currentGameState.player2.name, currentGameState.player2.score)
            mainLoop(None, currentGameState.player2, winner, nbRoundsToPlay - 1, currentGameState.random, nb_turn+1)
          }
          else {
            val winner = currentGameState.player2.copy(_score = currentGameState.player2.score + 1)
            Display.printWinner(winner.name)
            Display.printScore(winner.name, winner.score, currentGameState.player1.name, currentGameState.player1.score)
            mainLoop(None, currentGameState.player2, winner, nbRoundsToPlay - 1, currentGameState.random, nb_turn+1)
          }
        }
        else {
          mainLoop(Some(currentGameState), looser, winner, nbRoundsToPlay, currentGameState.random, nb_turn+1)
        }
    }
  }

}