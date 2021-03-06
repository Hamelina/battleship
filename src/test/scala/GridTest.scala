import game._
import org.scalatest._
import ship._

class GridTest extends FunSuite with DiagrammedAssertions {
  test(testName = "The size of the list should be equal to the size property of the grid") {
    val list1 = (1,2,3,4,5,6,7,8,9,10)
    val list2 = (1,2,3,4,5,6,7,8,9,10,11)
    //TODO create test

  }
  test(testName = "update a grid according to a state and a position") {
    val row1: List[String] = "occupied"::"not tested"::"missed"::"hit"::Nil
    val row2: List[String]  = "occupied"::"not tested"::"hit"::"hit"::Nil
    val updatedRow: List[String]  ="hit"::"not tested"::"hit"::"hit"::Nil
    val grid = Grid(List(row1, row2))
    val grid2 = Grid(List(row1, updatedRow))
    val gridUpdated = Grid.updateGrid(grid, 0,1, "hit")
    assert(gridUpdated==grid2)
  }
  test(testName = "check if a square have been targeted or not") {
    val row1: List[String] = "occupied"::"not tested"::"missed"::"hit"::Nil
    val row2: List[String]  = "occupied"::"not tested"::"hit"::"hit"::Nil
    val grid = Grid(List(row1, row2))
    val gridUpdated = Grid.updateGrid(grid, 1,0, "hit")
    assert(Utility.haveBeenTargeted(gridUpdated, Cell(1,0)))
  }


}

