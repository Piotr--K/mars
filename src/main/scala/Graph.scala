package Graph

//Plan:
//to calculate shortest path i will convert
//Grid to Graph, where each cell is a vertex
//and each edge is of the same weight eg. 1
//later on i will keep an array of cells which will be marked as mountenious
//they should be inaccessible from its neighbours, so initial graph
//need to be filtered to obtain 'working' graph

//todo: having graph i will use Dijsktra algorithm to find 
//      shortest path between 2 vertexes/cells
//todo - atm works for columns > 1  and rows > 1
final case class Cell(column: Int, row: Int) {
  def neighbours(grid: Grid): List[Cell] = {
    val maxColumn = grid.columns -1
    val maxRow = grid.rows -1
    (column, row) match {
      case (0, 0) => List(Cell(0,1), Cell(1,0), Cell(maxColumn,0), Cell(0,maxRow))
      case d if d._1 == 0 && d._2== maxRow => List(Cell(1,row), Cell(maxColumn,row), Cell(0,0), Cell(0,row -1))
      case (0, _) => List(Cell(1, row), Cell(maxColumn,row), Cell(column,row -1), Cell(column,row +1))
      case (maxColumn, 0) => List(Cell(0, 0), Cell(column -1, 0), Cell(column, 1), Cell(column, maxRow))
      case d if d._1 == maxColumn && d._2 == maxRow => List(Cell(0, row), Cell(column -1, row), Cell(column, 0), Cell(column, row -1))
      case (maxColumn, _) => List(Cell(0, row), Cell(column -1,row), Cell(column, row -1), Cell(column, row +1))
      case d if d._2 == 0 => List(Cell(column -1,row), Cell(column +1,row), Cell(column,1), Cell(column,maxRow))
      case d if d._2 == maxRow => List(Cell(column -1,row), Cell(column +1,row), Cell(column,0), Cell(column,row-1))
      case d if d._1 > 0 && d._1 < maxColumn && d._2 > 0 && d._2 < maxRow => List(Cell(column -1, row), Cell(column +1, row), Cell(column, row -1), Cell(column, row +1))
    }
  }
}

//graph is a map between cell and 4 adjacent cells - at best
//minus cells that are marked as mountanious
//to optimise Dijkstra alg. i would need to also eliminate
//duplicates, in current structure i will have the same edges added between vertexes twice
//but i think it shouldn't affect the result
case class Grid(columns: Int, rows: Int, badTerrain: List[Cell]) {
  def toGraph(): Map[Cell, List[Cell]] = {
    val cells: List[Cell] = {
      val colLst = (0 to columns -1).toList
      val rowLst = (0 to rows -1).toList
      for {
        column <- colLst
        row <- rowLst
      } yield Cell(column, row)
    } 
    cells.filter(cell => !badTerrain.contains(cell)).foldLeft[Map[Cell, List[Cell]]](Map.empty) {
      (acc, cell) => acc + (cell -> cell.neighbours(this).filter(c=> !badTerrain.contains(c)))
    }
  }
}