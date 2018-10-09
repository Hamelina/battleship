package config

/**
  * This object is where the configurations about the csv handler is stored
  */
object Config {

  val FILENAME: String = "./ai_proof.csv"
  val CSVSCHEMA = "AI Name;score;AI Name2;score2"
  val SIZE = 10

  //Size and number of the different ships allowed during the game
  val NUMBER_AND_SIZE_OF_SHIPS: List[List[Int]] = List(List(1, 1), List(1, 2), List(2, 3), List(1, 4), List(1, 5))
  //Maximum round that can be player between two players.
  val NUMBER_OF_TOTAL_ROUND: Int = 100

  /**
    * The different status of a square in a grid.
    */
  val OCCUPIED_STATUS = "occupied"
  val MISSED_STATUS = "missed"
  val HIT_STATUS = "hit"
  val NOT_TARGETED = "not targeted"
}
