import game._
import iohandler._
import player._

object Main extends App {


  Display.printWelcome

  gameBuilder()

  def gameBuilder(): Unit =
    Utility.askPlayerMode() match {
      case "Human mode" => {
        val player1Name: String = Utility.askForName("first")
        val player2Name: String = Utility.askForName("second")
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil))
        val player2: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = false, Grid(Nil))
        mainLoop(None, player1, player2, Game.NUMBER_OF_TOTAL_ROUND)
      }
      case "AI mode" => {
        val player1Name: String = Utility.askForName("first")
        val AIlevel: String = Utility.askForAILevel()
        val aiName: String = "AI" + AIlevel
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil))
        print(aiName)
        print(AIlevel.toInt)
        val player2: Player = Player(Nil, aiName, Some(AIlevel.toInt), 0, _isTurnToPlay =  false, Grid(Nil))
        mainLoop(None, player1, player2, Game.NUMBER_OF_TOTAL_ROUND)
      }
        //add here the little program that runs AI vs AI
      case _ => Display.printSomethingWentWrong
    }


  def mainLoop(gameState: Option[Game], looser: Player, winner: Player, nbRoundsToPlay: Int): Unit = {
    Utility.clearScreen()
    if (nbRoundsToPlay == 0) {
      println("Game finished")
      //TODO implement writeResultsIntoCsvFile function in game
      //gameState.getOrElse(Game(looser, winner).writeResultsIntoCsvFile("aiproof")
      Display.printScore(winner.name, winner.score, looser.name, looser.score)
    }
    else {
      val util: Utility = Utility(gameState.getOrElse(Utility.initializeRound(looser: Player, winner: Player)))

      //Ask the players to shoot
      val currentGameState: Game = util.askPlayerToShoot()

      //check is gameOver:
      if (currentGameState.isGameOver) {
        if (currentGameState.player1.hasLost) {
          val winner: Player = currentGameState.player1.copy(_score = currentGameState.player1.score + 1)
          Display.printVictory(winner.name)
          Display.printScore(winner.name, winner.score, currentGameState.player2.name, currentGameState.player2.score)
          mainLoop(None, currentGameState.player2, winner, nbRoundsToPlay - 1)
        }
        else {
          val winner = currentGameState.player2.copy(_score = currentGameState.player2.score + 1)
          Display.printWinner(winner.name)
          Display.printScore(winner.name, winner.score, currentGameState.player1.name, currentGameState.player1.score)
          mainLoop(None, currentGameState.player2, winner, nbRoundsToPlay - 1)
        }
      }
      else {
        mainLoop(Some(currentGameState), looser, winner, nbRoundsToPlay)
      }
    }
  }
}