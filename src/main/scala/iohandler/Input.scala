package iohandler

import scala.io.StdIn.readLine

//TODO
class Input() {




}
object Input{
  def getShootCoordinates: String = readLine
  //TODO convert starting point A letter to number so --> 0 0 R --> A 0 R
  def startingPoint: String = readLine
  def name: String = readLine
  def level: String = readLine
  def mode: String = readLine
}
