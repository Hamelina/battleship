import game.{Grid, Utility}
import org.scalatest._
import player._
import ship._

class PlayerTest extends FunSuite with DiagrammedAssertions {
  test(testName = "PlayerTest should get the name, the fleet and the level of a player"){
    var cell1 = Cell(0, 0)
    var cell2 = Cell( 1, 2)
    var list = List(cell1, cell2)
    var ship = Ship(_size = list.size, _cells = list)
    var fleet = Nil
    /*"Occupied" -> X
    * "Miss" -> T
    * "Hit" -> H
    * "Not tested" -> O
    */
    var player = Player(_fleet = ship+:fleet, _name = Option("Player"), _level= None ,
      _score = 0, _isTurnToPlay = false, _gridStates = Grid(List(List(Utility.OCCUPIED_STATUS,Utility.OCCUPIED_STATUS,Utility.MISSED_STATUS), List(Utility.HIT_STATUS,Utility.HIT_STATUS,Utility.NOT_TARGETED))))

    assert(player.isInstanceOf[Player])
  }

  //TODO : tester hasLost
  //TODO : tester numberOfShipLeft
}

