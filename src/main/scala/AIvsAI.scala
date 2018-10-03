

/**
  * This is a programm that gives proof of the assertion
  */
case class AIvsAI() {

}
//to transform in grid
/*def initializeGridFromFleet(fleet: List[Ship], size: Int): List[List[String]] = {
  val emptyGrid: List[List[String]] = List(List(""))
  val cells: List[Cell] = (fleet.map( x => x.cells)).flatMap(x => x)
  createStatusLists(cells = cells, line = 0, statusList = emptyGrid)

  @tailrec
  def createStatusLists(cells: List[Cell], line: Int, statusList: List[List[String]]): List[List[String]] = {

    if (line >= size - 1 && statusList.size >= size-1) {
      statusList
    }
    else{
      createStatusLists(cells, line+1, statusList = statusList:+fillListOfLines(line+1, 0, size, cells, Nil))
    }
    @tailrec
    def fillListOfLines(line: Int, column: Int, size: Int, cells: List[Cell], list: List[String]): List[String] = {
      if (column <= size-1 && cells.contains(Cell(line, size))) {
        fillListOfLines(line, column + 1, size, cells, list = list :+ Utility.OCCUPIED_STATUS)
      }
      else if (!cells.contains(Cell(line, size))) {fillListOfLines(line, column + 1, size, cells, list = list :+ Utility.NOT_TARGETED)}
      else list
    }
  }
}*/