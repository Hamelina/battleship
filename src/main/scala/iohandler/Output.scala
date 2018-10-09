package iohandler

import java.io.{BufferedWriter, FileWriter}

import config._


/**
  *An object that is in charge of the output of the battleship game.
  */
object Output{

  def writeResult(records:String): Unit = {
    val bufferWrite = new BufferedWriter(new FileWriter(Config.FILENAME))
    bufferWrite.write(Config.CSVSCHEMA+"\n")
    //val score: String = game.player1.name+";"+game.player1.score.toString+";"+game.player2.name+";"+game.player1.score.toString+"\n"
    bufferWrite.write(records)
    bufferWrite.close()
  }
}
