import game._
import player._

object Main extends App {


  println("Welcome to you ! You have started the most enjoyable game ever: the battleship !")

  gameBuilder

  def gameBuilder: Unit =
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
        val aiName: String = "AI" + Utility.askForAILevel()
        val player1: Player = Player(Nil, player1Name, None, 0, _isTurnToPlay = true, Grid(Nil))
        val player2: Player = Player(Nil, aiName, Some(aiName.substring(2, 3).toInt), 0, _isTurnToPlay =  false, Grid(Nil))
        mainLoop(None, player1, player2, Game.NUMBER_OF_TOTAL_ROUND)
      }
        //add here the little program that runs AI vs AI
      case _ => println("Something went wrong. The source code must have been changed")
    }


  def mainLoop(gameState: Option[Game], looser: Player, winner: Player, nbRoundsToPlay: Int): Unit = {
    Utility.clearScreen()
    if (nbRoundsToPlay == 0) {
      println("Game finished")
      val finalGameState = Game(winner, looser)
      //TODO implement writeResultsIntoCsvFile function in game
      //finalGameState.writeResultsIntoCsvFile("aiproof")
      println("Score: " + winner.name + ": " + winner.score + " VS " + looser.name + ": " + looser.score)
    }
    else {
      var newGameState: Option[Game] = gameState
      if (gameState.isEmpty) {
        newGameState = Utility.initializeRound(looser: Player, winner: Player)
      }
      val util = Utility(newGameState.get)

      //Ask the players to shoot
      val currentGameState = util.askPlayersToShoot

      //check is gameOver:
      if (currentGameState.isGameOver) {
        if (currentGameState.player1.hasLost) {
          val winner: Player = currentGameState.player1.copy(_score = currentGameState.player1.score + 1)
          println("Congratulations to " + winner.name + ", you won this round ! ")
          println("Score: " + winner.name + ": " + winner.score + " VS " + currentGameState.player2.name + ": " + currentGameState.player2.score)
          mainLoop(None, currentGameState.player2, winner, nbRoundsToPlay - 1)
        }
        else {
          val winner = currentGameState.player2.copy(_score = currentGameState.player2.score + 1)
          println("Congratulations to " + winner.name + ", you won this round ! ")
          println("Score: " + winner.name + ": " + winner.score + " VS " + currentGameState.player1.name + ": " + currentGameState.player1.score)
          mainLoop(None, currentGameState.player2, winner, nbRoundsToPlay - 1)
        }
      }
      else {
        mainLoop(Some(currentGameState), looser, winner, nbRoundsToPlay)
      }
    }
  }
}







  /**
    * create game with player1 and player 2
    * val gameRecord = Nil
    *
    * mainloop(Option[game], gameRecords)
    *
    * mainLoop(gameState: Game, gameRecords)
    *   console.clear
    *   if(game){
      *   if gameState.player1.hasLost || gameState.player2.hasLost (end of game)
      *     printGameOver
      *     continue : ReadLine (Do you still want to play ? )
      *     val record: GameRecords;
      *     if gameState.player1.hasLost{
      *       record = Game(gameState.player2.copy(score=player2.score+1), gameState.player1) +: gamerecord
      *       }
      *     else {
      *       record = Game(player1.copy(score=player1.score+1), gameState.player2) +: gamerecord
      *     }
      *     if (continue){
      *       mainLoop(None, record)
      *     }
      *     else prinln("Partie over", player1.name won .../...)
      *  else{
    *       game.renderGridToUser
      *     val newGame: Game = Utility.askUserForShoot(gameState)
    *       Console.clear
    *       renderGridToUser
    *       mainloop(newGame, gameRecords)
    *     }
    *
    *
    *   }
    */





  /*Init 2 grid
  *
  * */


  /*TO DO: lui demander de rentrer les positions de chaque bateau:
  *
  * */


  //Utility.askUserForShipSettings(Utility.NUMBER_AND_SIZE_OF_SHIPS, Nil,"Bill")
  //var ship = Utility.askUserForShipSettings(3, Nil)
  //var grid = initializeGridFromFleet(ship, 10)
  //println(grid)

  //createGridFromFleet(ship)

/*println("You have the possibility to play either against another player or against 3 levels of Artificial Intelligence. If you want to play against: ")
  println("another player --> press 0.")
  println("AI level 1 --> press 1.")
  println("AI level 2 --> press 2." )
  println("AI level 3 --> press 3.")
  val input = scala.io.StdIn.readLine("Please enter your game mode: ")
  input match {
    case "0" => println("You decide to play against another player")
    case "1" => println("You decide to play against AI level 1 ")
    case "2" => println("You decide to play against AI level 2")
    case "3" => println("You decide to play against AI level 3")
    case _ => println("Your input was not expected, sorry")
  }
  val player1Name = scala.io.StdIn.readLine("Please enter the name of the first player who will start the game: ")
  val player2Name = scala.io.StdIn.readLine("Please enter the name of the other player: ")

  println("----Player 1: "+player1Name+"********* Player 2:"+player2Name)*/

/*
  /*val player1Name = "Player 1"
  val player2Name = "Player 2"

  val levelPlayer1: Option[Int] = None
  val levelPlayer2: Option[Int] = None




  val grid: Grid = Grid(Nil)
 // private val _fleet: List[Ship], private val _name: String, private val _level: Option[Int], private val _score: Int, private val _isTurnToPlay: Boolean, private val _gridStates: Grid
  val player1 = Player(Nil, player1Name, levelPlayer1, 0, true, Grid(Nil))
  println("******* PLAYER 1 CREATE *******")
  val player2 = Player(Nil, player2Name, levelPlayer1, 0, false, Grid(Nil))
  println("******* PLAYER 2 CREATE *******")
  println(player2)
  val round = Game(player1, player2)
  println(player1)
  val nb=1
  mainLoop(None, this.player1, this.player2, nb)*/
 */
