package iohandler

import scala.io.StdIn.readLine

  /**
  * Object in charge of the asking for inputs.
  */
object Input{
    /**
      * Retrieve the input of the user concerning the number of total round he/she wants to play.
      * @return A string that corresponds to the number of total round he/she wants to play.
      */
  def getNumberOfRound: String = readLine

    /**
      * Retrieve the input of the user concerning the coordinates of the square to target.
      * @return A string that corresponds to coordinates of the square to target
      */
  def getShootCoordinates: String = readLine

    /**
      * Retrieve the input of the user concerning the coordinates of starting point of a ship.
      * @return A string that corresponds to coordinates of starting point of a ship
      */
  def startingPoint: String = readLine

    /**
      * Retrieve the name of the user who is being asked to.
      * @return A string that corresponds to the name of the user who is being asked to
      */
  def name: String = readLine

    /**
      * Retrieve the level of AI the user wants to challenge.
      * @return A string that corresponds to the level of AI the user wants to challenge.
      */
  def level: String = readLine

    /**
      * Retrieve the game mode the user wants to turn on.
      * @return A string that corresponds to the game mode the user wants to turn on.
      */
  def mode: String = readLine
}
