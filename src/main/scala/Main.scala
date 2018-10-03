import game._


object Main extends App{
  /*def cube(x: Int) = {
    x * x * x
  }*/
  println("Welcome to you ! You have started the most enjoyable game ever: the battleship !")
  /*val playerType = scala.io.StdIn.readLine("Are you an AI ? 'yes' or 'no'") match {
    case "yes" => true
    case "no" => false
    case _ => null
  }*/
 println("You have the possibility to play either against another player or against 3 levels of Artificial Intelligence. If you want to play against: ")
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

  println("----Player 1: "+player1Name+"********* Player 2:"+player2Name)


  val player1 = Game.createPlayer(player1Name, isTurnToPlay = true)
  val player2 = Game.createPlayer(player2Name, isTurnToPlay = false)
  val round = Game(player1, player2)
  println("******* PLAYER 1 CREATE *******")
  println(player1)

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

}
