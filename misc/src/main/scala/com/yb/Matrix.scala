package com.yb

object Matrix {



  def main(args: Array[String]): Unit = {
    val m = Array(Array(1,2,3), Array(4,5,6), Array(7,8,9))
    clockwise(m)
  }

  def clockwise(mat: Array[Array[Int]]) = {
    val rows = mat.length -1
    val cells = mat(0).size - 1

    for(i <- 0 to rows;j <- 0 to cells){
      val currentPosition = Position(i, j)
      val newPostion = clockWisePoint(currentPosition, rows, cells)
      println(s"old: [$i, $j] => [${newPostion.x}, ${newPostion.y}]]")
    }

  }

  case class Position(x: Int, y: Int)

  def clockWisePoint(p : Position, xSize: Int, ySize: Int): Position = {
    val midX = xSize / 2
    val midY = ySize / 2

    val newPosition =
      //should pad right
      if(p.x <= p.y && p.y < ySize - p.x){
        println("RIGHT")
        Position(p.x, p.y + 1)
      }
      //should pad down
      else if(p.x < p.y && p.x > midX){
        println("DOWN")
        Position(p.x + 1, p.y)
      }
      //should pad left
      else if(p.x >= p.y && p.x > midX){
        println("LEFT")
        Position(p.x + 1, p.y)
      }
      //should pad up
      else if(p.x > p.y && p.x <= midX){
        println("UP")
        Position(p.x - 1, p.y)
      }
    else{
        Position(-1, -1)
      }
    newPosition
  }

}
