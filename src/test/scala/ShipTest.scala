import game._
import org.scalatest._
import ship._


class ShipTest extends FunSuite with DiagrammedAssertions {
  val cell1 = Cell(1, 1)
  val cell2 = Cell( 1, 2)
  val list = List(cell2, cell1)
  val ship = Ship(_typeName = "Carrier", _cells = list)
  test(testName = "a ship should be created"){
    assert(ship.isInstanceOf[Ship])
  }
  test(testName = "get the cells of a ship"){
    assert(ship.cells==list)
  }
  test(testName = "should return the initial type of the ship"){
    assert(ship.typeName=="Carrier")
  }
  test(testName = "should return if a ship is sunk or not"){
    assert(!ship.isSunk)
    assert(!Ship(_typeName = "Carrier",_cells = list.tail).isSunk)
    assert(Ship(_typeName = "Carrier",_cells = list.tail.tail).isSunk)
  }
  test(testName = "the method Ship.createShip should create an identical ship as the val ship"){
    //create ship by calling the function
    val ship2 = Ship.createShip(startPoint = Cell(1,1),shipType = "Carrier", shipSize = 2, direction = "right")
    //assert if the ship are the same
    assert(ship.cells==ship2.cells)
    assert(ship.typeName==ship2.typeName)
  }
  test(testName = "should say whether a ship can or cannot be built on an empty grid"){
    //this ship can be built
    assert(Ship.isValid(x = 0,y = 3, size = 1, direction = "up"))

    //test the corner
    assert(!Ship.isValid(x = 0,y = 0, size = 2, direction = "up"))
    assert(Ship.isValid(x = 0,y = 0, size = 2, direction = "down"))
    assert(!Ship.isValid(x = 0,y = 0, size = 2, direction = "left"))
    assert(Ship.isValid(x = 0,y = 0, size = 2, direction = "right"))

    assert(Ship.isValid(x = Grid.limitX ,y = Grid.limitY , size = 2, direction = "up"))
    assert(!Ship.isValid(x = Grid.limitX,y = Grid.limitY, size = 2, direction = "down"))
    assert(Ship.isValid(x = Grid.limitX,y = Grid.limitY, size = 2, direction = "left"))
    assert(!Ship.isValid(x = Grid.limitX,y = Grid.limitY, size = 2, direction = "right"))

    assert(Ship.isValid(x = 0,y = Grid.limitY, size = 2, direction = "up"))
    assert(!Ship.isValid(x = 0,y = Grid.limitY, size = 2, direction = "down"))
    assert(!Ship.isValid(x = 0,y = Grid.limitY, size = 2, direction = "left"))
    assert(Ship.isValid(x = 0,y = Grid.limitY, size = 2, direction = "right"))

    assert(!Ship.isValid(x = Grid.limitX,y = 0, size = 2, direction = "up"))
    assert(Ship.isValid(x = Grid.limitX,y = 0, size = 2, direction = "down"))
    assert(Ship.isValid(x = Grid.limitX,y = 0, size = 2, direction = "left"))
    assert(!Ship.isValid(x = Grid.limitX,y = 0, size = 2, direction = "right"))

    //this ship can be built
    assert(Ship.isValid(x = 3,y = 3, size = 1, direction = "up"))

    //For a ship of size 1, makes sur the ship can be built
    assert(Ship.isValid(x = 1,y = 1, size = 1, direction = "up"))
    assert(Ship.isValid(x = 1,y = 1, size = 1, direction = "down"))
    assert(Ship.isValid(x = 1,y = 1, size = 1, direction = "left"))
    assert(Ship.isValid(x = 1,y = 1, size = 1, direction = "right"))

    //Statement not valid
    assert(!Ship.isValid(x = 1,y = 1, size = 1, direction = "Not valid"))
  }
  test(testName = "should return whether a ship shares at least one cell with another ship or not"){
    val ship2 = Ship.createShip(startPoint = Cell(1,1),shipType = "Carrier", shipSize = 2, direction = "right")
    val ship3 = Ship.createShip(startPoint = Cell(2,2),shipType = "Carrier", shipSize = 2, direction = "right")

    assert(ship.isSuperposedTo(ship2))
    assert(!ship.isSuperposedTo(ship3))

  }

}

