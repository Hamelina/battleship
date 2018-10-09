import game._
import org.scalatest._
import ship._


class ShipTest extends FunSuite with DiagrammedAssertions {
  val cell1 = Cell(1, 1)
  val cell2 = Cell( 1, 2)
  val list = List(cell2, cell1)
  val ship = Ship(_size = list.size, _cells = list)
  test(testName = "a ship should be created"){
    assert(ship.isInstanceOf[Ship])
  }
  test(testName = "get the cells of a ship"){
    assert(ship.cells==list)
  }
  test(testName = "should return the initial type of the ship"){
    assert(ship.size == list.size)
  }
  test(testName = "should return if a ship is sunk or not"){
    assert(!ship.isSunk)
    assert(!Ship(_size = list.size,_cells = list.tail).isSunk)
    assert(Ship(_size = list.tail.tail.size, list.tail.tail).isSunk)
  }
  test(testName = "the method Ship.createShip should create an identical ship as the val ship"){
    //create ship by calling the function
    val ship2 = Ship.createShipFromStartingPoint(startPoint = Cell(1,1), shipSize = 2, direction = "R")
    val c = Cell(2,1)::Cell(1,1)::Nil
    val ship1 = Ship(_size = c.size, _cells = c)
    //assert if the ship are the same
    assert(ship1.cells==ship2.cells)
    assert(ship1.size==ship2.size)
  }
  test(testName = "should say whether a ship can or cannot be built on an empty grid"){
    //this ship can be built
    assert(Ship.isValid(x = 0,y = 3, size = 1, direction = "U"))

    //test the corner
    assert(!Ship.isValid(x = 0,y = 0, size = 2, direction = "U"))
    assert(Ship.isValid(x = 0,y = 0, size = 2, direction = "D"))
    assert(!Ship.isValid(x = 0,y = 0, size = 2, direction = "L"))
    assert(Ship.isValid(x = 0,y = 0, size = 2, direction = "R"))

    assert(Ship.isValid(x = Grid.SIZE-1 ,y = Grid.SIZE-1 , size = 2, direction = "U"))
    assert(!Ship.isValid(x = Grid.SIZE-1,y = Grid.SIZE-1, size = 2, direction = "D"))
    assert(Ship.isValid(x = Grid.SIZE-1,y = Grid.SIZE-1, size = 2, direction = "L"))
    assert(!Ship.isValid(x = Grid.SIZE-1,y = Grid.SIZE-1, size = 2, direction = "R"))

    assert(Ship.isValid(x = 0,y = Grid.SIZE-1, size = 2, direction = "U"))
    assert(!Ship.isValid(x = 0,y = Grid.SIZE-1, size = 2, direction = "D"))
    assert(!Ship.isValid(x = 0,y = Grid.SIZE-1, size = 2, direction = "L"))
    assert(Ship.isValid(x = 0,y = Grid.SIZE-1, size = 2, direction = "R"))

    assert(!Ship.isValid(x = Grid.SIZE-1,y = 0, size = 2, direction = "U"))
    assert(Ship.isValid(x = Grid.SIZE-1,y = 0, size = 2, direction = "D"))
    assert(Ship.isValid(x = Grid.SIZE-1,y = 0, size = 2, direction = "L"))
    assert(!Ship.isValid(x = Grid.SIZE-1,y = 0, size = 2, direction = "R"))

    //this ship can be built
    assert(Ship.isValid(x = 3,y = 3, size = 1, direction = "U"))

    //For a ship of size 1, makes sur the ship can be built
    assert(Ship.isValid(x = 1,y = 1, size = 1, direction = "U"))
    assert(Ship.isValid(x = 1,y = 1, size = 1, direction = "D"))
    assert(Ship.isValid(x = 1,y = 1, size = 1, direction = "L"))
    assert(Ship.isValid(x = 1,y = 1, size = 1, direction = "R"))

    //Statement not valid
    assert(!Ship.isValid(x = 1,y = 1, size = 1, direction = "Not valid"))
  }
  test(testName = "should return whether a ship shares at least one cell with another ship or not"){
    val ship2 = Ship.createShipFromStartingPoint(startPoint = Cell(1,1), shipSize = 2, direction = "R")
    val ship3 = Ship.createShipFromStartingPoint(startPoint = Cell(2,2), shipSize = 2, direction = "R")

    assert(ship.isSuperposedTo(ship2))
    assert(!ship.isSuperposedTo(ship3))

  }
  test(testName = "test the function hasAtLeastOneElement of the object Ship"){
    val l1 = List(List(Cell(1,2), Cell(1,3)), List(Cell(4,5), Cell(4, 6)))
    val l2 = Cell(1, 1)::Nil
    val l3 = Cell(4,5) :: Cell(1,1) :: Nil
    assert(!Ship.hasAtLeastOneElement(l1, l2))
    assert(Ship.hasAtLeastOneElement(l1, l3))
  }
  test(testName = "test the createCellsList function "){
    val l1 = List(List(Cell(1,2), Cell(1,3)), List(Cell(4,5), Cell(4, 6)))
    val l2 = Cell(2, 1)::Cell(1,1)::Nil
    val l2bis = Cell(1, 1)::Cell(1,2)::Nil
    val l3 = Cell(4,5) :: Cell(1,1) :: Nil

    assert(Ship.createCellsList(2, "R", Cell(1,1)::Nil) == l2)
    assert(Ship.createCellsList(2, "R", Cell(1,1)::Nil) != l2bis)
    assert(Ship.createCellsList(1, "R", Cell(1,1)::Nil) != l2)
    assert(Ship.createCellsList(2, "R", Cell(1,1)::Nil) != l3)

    assert(Ship.createCellsList(1, "R", Cell(1,1)::Nil) != Cell(4,5) :: Cell(1,1) :: Nil)

    assert(Ship.createCellsList(1, "R", Cell(1,1)::Nil) == Cell(1,1)::Nil)
  }



}

