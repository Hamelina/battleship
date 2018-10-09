import org.scalatest._
import player.Player
import ship._
import game._

class PlayerTest extends FunSuite with DiagrammedAssertions {
  test(testName = "PlayerTest should get the name, the fleet and the level of a player"){
    val cell1 = Cell(0, 0)
    val cell2 = Cell( 1, 2)
    val list = List(cell1, cell2)
    val ship = Ship(_size = list.size, _cells = list)
    val fleet = Nil
    /*"Occupied" -> X
    * "Miss" -> T
    * "Hit" -> H
    * "Not tested" -> O
    */
    //var player = Player(_fleet = ship+:fleet, _name = "Player", _level= None , _score = 0, _isTurnToPlay = false, _testedDirection = None, _gridStates = Grid(List(List(Utility.OCCUPIED_STATUS,Utility.OCCUPIED_STATUS,Utility.MISSED_STATUS), List(Utility.HIT_STATUS,Utility.HIT_STATUS,Utility.NOT_TARGETED))), _currentDirection = None, _lastHit = None, _random = Some(new Random))

    //assert(player.isInstanceOf[Player])
  }

  test(testName = "test of a shoot"){
    val cell1 = Cell(0, 0)
    val cell2 = Cell( 1, 2)
    val list = List(cell1, cell2)
    val ship = Ship(_size = list.size, _cells = list)
    val fleet: List[Ship] = ship::Nil
    val grid: Grid = Grid.initializeGridFromFleet(fleet, 5)
    val player: Player = Player(_fleet = fleet, _name = "Test", _level = None, _score = 0, _isTurnToPlay = true, _gridStates = grid, _targeted = Nil, _currentDirection = None, _random = None, _lastHit = None)

    val grid2: Grid = Grid.updateGrid(grid=grid, x=cell1.x, y=cell1.y, "hit")
    assert(Utility.shoot(player, cell1.x, cell1.y).gridStates==grid2)
  }

}

