import Mars._

class RoverTest extends org.scalatest.funsuite.AnyFunSuite {
  implicit val grid = new Grid (10, 10)
  test("Rover should move by one cell") {
    assert(Rover.move(new Position(0, 0, E)) === new Position(1, 0, E))
    assert(Rover.move(new Position(0, 0, S)) === new Position(0, 1, S))
  }

  test("Moves on the edges of the grid should wrapp around") {
    assert(Rover.move(new Position(0, 0, N)) === new Position(0, 9, N))
    assert(Rover.move(new Position(0, 0, W)) === new Position(9, 0, W))
    assert(Rover.move(new Position(9, 0, E)) === new Position(0, 0, E))
    assert(Rover.move(new Position(0, 9, S)) === new Position(0, 0, S))
  }
}
