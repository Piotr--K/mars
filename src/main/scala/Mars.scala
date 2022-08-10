package Mars

import javax.print.attribute.standard.PresentationDirection

object Mars extends App {
}

case class Position (column: Int, row: Int, currentDirection: Direction)

trait Direction {
  def rotate(req: RotateDirection): Direction
}
case object N extends Direction {
  def rotate(req: RotateDirection): Direction = {
    req match {
      case L => W
      case R => E
    }
  }
}
case object E extends Direction {
  def rotate(req: RotateDirection): Direction = {
    req match {
      case L => N
      case R => S
    }
  }
}
case object W extends Direction {
  def rotate(req: RotateDirection): Direction = {
    req match {
      case L => S
      case R => N
    }
  }
}
case object S extends Direction {
  def rotate(req: RotateDirection): Direction = {
    req match {
      case L => E
      case R => W
    }
  }
}

trait RotateDirection
final case object L extends RotateDirection
final case object R extends RotateDirection

//todo: limit to just positive numbers
final case class Grid(rows: Int, columns: Int)


object Rover {
  def moves(req: String): String = {
    ""
  }
  def rotate(initialPosition: Position, req: RotateDirection): Position = 
    new Position(
        initialPosition.column, 
        initialPosition.row, 
        initialPosition.currentDirection.rotate(req))
  
  def move (iniP: Position)(implicit grid: Grid): Position = {
    iniP.currentDirection match {
      case N => if(iniP.row < 0) {
        new Position(iniP.column, iniP.row - 1, iniP.currentDirection)
      } else {
        new Position(iniP.column, grid.rows - 1, iniP.currentDirection)
      }
      case E => if(iniP.column < grid.columns - 1) {
        new Position(iniP.column + 1, iniP.row, iniP.currentDirection)
      } else {
        new Position(0, iniP.row, iniP.currentDirection)
      }
      case W => if(iniP.column > 0) {
        new Position(iniP.column -1, iniP.row, iniP.currentDirection)
      } else {
        new Position(grid.columns - 1, iniP.row, iniP.currentDirection)
      }
      case S => if(iniP.row < 9) {
        new Position(iniP.column, iniP.row + 1, iniP.currentDirection)
      } else {
        new Position(iniP.column, 0, iniP.currentDirection)
      }
    }
  }
}

