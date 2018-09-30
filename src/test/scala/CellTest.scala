import org.scalatest._
import ship._

class CellTest extends FunSuite with DiagrammedAssertions {
  test(testName = "CellTest should get x and y so return 0 an 0 argument"){
    var cell = Cell(0, 0)
    assert(cell.x ==0)
    assert(cell.y ==0)
  }
  test("Test the validity of a point as a cell"){
    assert(!Cell.isValid(-1,9))
    assert(!Cell.isValid(2,-1))
    assert(!Cell.isValid(-1,-1))
    assert(!Cell.isValid(20,9))
    assert(!Cell.isValid(9,20))
    assert(Cell.isValid(1,1))
    assert(Cell.isValid(10,10))
    assert(Cell.isValid(5,5))
  }
}

