package com.yb

object HackerRankMatrix {
  def main(args: Array[String]): Unit = {
    import Utils.HackerRankUtils
    /*
        val grid1 =
          """7652157548860692421022503
            |9283597467877865303553675
            |4160389485250089289309493
            |2583470721457150497569300
            |3220130778636571709490905
            |3588873017660047694725749
            |9288991387848870159567061
            |4840101673383478700737237
            |8430916536880190158229898
            |8986106490042260460547150
            |2591460395957631878779378
            |1816190871689680423501920
            |0704047294563387014281341
            |8544774664056811258209321
            |9609294756392563447060526
            |0170173859593369054590795
            |6088985673796975810221577
            |7738800757919472437622349
            |5474120045253009653348388
            |3930491401877849249410013
            |1486477041403746396925337
            |2955579022827592919878713
            |2625547961868100985291514
            |3673299809851325174555652
            |4533398973801647859680907""".stringToArrayOfString
        val pat1 =
          """5250
            |1457
            |8636
            |7660
            |7848""".stringToArrayOfString
        gridSearch(grid1, pat1)
        */

    val matrix = Array(
      Array(1, 2, 3, 4, 5),
      Array(16, 1, 2, 3, 6),
      Array(15, 8, 0, 4, 7),
      Array(14, 7, 6, 5, 8),
      Array(13, 12, 11, 10, 9)
    )
    matrix.foreach(a => println(a.mkString(" . ")))
    this.matrixRotation(matrix, 4992211)
    println("----------------------------------------------------")
    println("----------------------------------------------------")
    matrix.foreach(a => println(a.mkString(" . ")))
  }

  def matrixRotation(matrix: Array[Array[Int]], r: Int) {

    def nonEmptyMatrix(xS: Int, xE: Int, yS: Int, yE: Int): Boolean = {
      if (xE - xS > 0 && yE - yS > 0)
        true
      else
        false
    }

    def nextMatrix(xS: Int, xE: Int, yS: Int, yE: Int): (Int, Int, Int, Int) = {
      (xS + 1, xE - 1, yS + 1, yE - 1)
    }

    def findCoordinates(m: Array[Array[Int]]): (Int, Int, Int, Int) = {
      val xE = m.length - 1
      val xS = 0
      val yE = m(0).length - 1
      val yS = 0

      (xS, xE, yS, yE)
    }

    //not working on 2x2 matrix
    def clockwise(xS: Int, xE: Int, yS: Int, yE: Int, matrix: Array[Array[Int]]): Unit = {
      var previous = matrix(xS + 1)(yS)
      var current = 0
      //for the North
      for (col <- yS to yE - 1) {
        current = matrix(xS)(col)
        matrix(xS)(col) = previous
        previous = current
      }
      //for the East
      for (row <- xS to xE - 1) {
        current = matrix(row)(yE)
        matrix(row)(yE) = previous
        previous = current
      }
      //for the South
      for (col <- yE to yS + 1 by -1) {
        current = matrix(xE)(col)
        matrix(xE)(col) = previous
        previous = current
      }
      //for the West
      for (row <- xE to xS + 1 by -1) {
        current = matrix(row)(yS)
        matrix(row)(yS) = previous
        previous = current
      }
    }

    def antiClockwise(xS: Int, xE: Int, yS: Int, yE: Int, matrix: Array[Array[Int]]): Unit = {
      var lastValToSet = matrix(xS)(yS)
      //for the North
      for (col <- yS to yE - 1) {
        matrix(xS)(col) = matrix(xS)(col + 1)
      }
      //for the East
      for (row <- xS to xE - 1) {
        matrix(row)(yE) = matrix(row + 1)(yE)
      }
      //for the South
      for (col <- yE to yS + 1 by -1) {
        matrix(xE)(col) = matrix(xE)(col - 1)
      }
      //for the West
      for (row <- xE to xS + 1 by -1) {
        matrix(row)(yS) = matrix(row - 1)(yS)
      }
      matrix(xS + 1)(yS) = lastValToSet
    }

    var coord: (Int, Int, Int, Int) = findCoordinates(matrix)
    var doClockwise = false

    for (rotate <- 1 to r) {
      while (nonEmptyMatrix(coord._1, coord._2, coord._3, coord._4)) {
        if (doClockwise)
          clockwise(coord._1, coord._2, coord._3, coord._4, matrix)
        else
          antiClockwise(coord._1, coord._2, coord._3, coord._4, matrix)

        coord = nextMatrix(coord._1, coord._2, coord._3, coord._4)
        //        doClockwise = !doClockwise
      }
      coord = findCoordinates(matrix)
    }
    matrix.foreach(a => println(a.mkString(" ")))
  }
}
