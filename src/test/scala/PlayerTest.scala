import org.scalatest._
import ship._
import player._

class PlayerTest extends FunSuite with DiagrammedAssertions {
  test(testName = "PlayerTest should get the name, the fleet and the level of a player"){
    var cell1 = Cell(0, 0)
    var cell2 = Cell( 1, 2)
    var list = List(cell1, cell2)
    var ship = Ship(_typeName = "Carrier", _cells = list)
    var fleet = Nil
    var player = Player(_fleet = ship+:fleet, _name = Option("Player"), _level= None , _score = 0)
    assert(player.isInstanceOf[Player])
  }
}

