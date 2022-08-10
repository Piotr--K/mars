import Graph._

class Graph extends org.scalatest.funsuite.AnyFunSuite  {
  test("Graph cell 0,0 should have 4 neighbours") {
    val graph = new Grid(3, 3, List.empty).toGraph()
    println(graph)
    assert(graph.get(Cell(0,0)) == Some(List(Cell(0,1), Cell(1,0), Cell(2,0), Cell(0,2))))
  }
  test("Graph cell 0,1 should have 4 neighbours") {
    val graph = new Grid(3, 3, List.empty).toGraph()
    println(graph)
    assert(graph.get(Cell(0,1)) == Some(List(Cell(1,1), Cell(2,1), Cell(0,0), Cell(0,2))))
  }
}
