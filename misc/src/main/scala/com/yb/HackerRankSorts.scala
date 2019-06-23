package com.yb

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.control.Breaks

object Utils {

  implicit class HackerRankUtils(val s: String) {
    def stringToArrayOfString = s.stripMargin.split("\r\n").toArray
  }

}

object HackerRankSorts {

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

  def organizingContainers(container: Array[Array[Int]]): String = {

    def isMatrixSwappable: Boolean = {
      val rows = container.length
      val sumMatrix = container.flatten.sum
      sumMatrix % rows == 0
    }

    def canSwapAtLeastOneType(contA: Array[Int], contB: Array[Int]): Unit = {
      val maxTypeA = contA.max
      for (
        typeA <- 0 until contA.length;
        typeB <- 1 until contB.length
      ) yield {
        contA(typeA) == contB(typeB)
      }
    }

    ""
  }

  // Complete the equalizeArray function below.
  def equalizeArray(arr: Array[Int]): Int = {
    val morePresentValue = arr.foldLeft(Map.empty[Int, Int]) {
      (map, value) => {
        map + (value -> (map.getOrElse(value, 0) + 1))
      }
    }.maxBy(_._2)._1

    //less to be removed
    morePresentValue - arr.length
  }

  def findDigits(n: Int): Int = {
    n.toString.map {
      ds => {
        val d = ds.toString.toInt
        println(s"$ds => $d")
        if (d != 0 && n % d == 0)
          1
        else
          0
      }
    }
      .sum
  }

  // Complete the gridSearch function below.
  def gridSearch(grid: Array[String], pattern: Array[String]): String = {
    var resultFound = false
    val rows = grid.length - pattern.length
    val cols = grid(0).length


    val loop = new Breaks()
    loop.breakable {

      for (
        rowIdx <- 0 until rows;
        chrIdx <- 0 until cols
      ) {
        val curChar = grid(rowIdx)(chrIdx)
        //start matching attempt
        if (curChar == pattern(0)(0)) {
          var allMatch = true
          val startIndex = chrIdx

          for (nextRowIdx <- 1 until pattern.length) {
            val nextLine = grid(rowIdx + nextRowIdx)
            println(s" compare [${nextLine}] at row [${rowIdx + nextRowIdx}] with [${pattern(nextRowIdx)}] at row [${nextRowIdx}]")
            if (startIndex != nextLine.indexOf(pattern(nextRowIdx))) {
              allMatch = false
              loop.break()
            }
          }
          if (allMatch) {
            resultFound = true
            loop.break()
          }
        }
      }
    }
    if (resultFound)
      "YES"
    else
      "NO"
  }

  // Complete the gridSearch function below.
  def gridSearch2(grid: Array[String], pattern: Array[String]): String = {
    var resultFound = false
    val rows = grid.length - pattern.length


    val loop = new Breaks()
    loop.breakable {

      for (rowIdx <- 0 until rows) {
        val line = grid(rowIdx)
        //start to search on next lines for pattern
        if (line.contains(pattern(0))) {
          println("---***")
          pattern(0).r.findAllMatchIn(line).map(_.start).foreach { startIndex =>
            println("---***")
            var allMatch = true
            //              val startIndex = line.indexOf(pattern(0))
            println(s"!!! Match found [${line}] contains [${pattern(0)}] at index [$startIndex]")
            for (nextRowIdx <- 1 until pattern.length) {
              val nextLine = grid(rowIdx + nextRowIdx)
              println(s" compare [${nextLine}] at row [${rowIdx + nextRowIdx}] with [${pattern(nextRowIdx)}] at row [${nextRowIdx}]")
              if (startIndex != nextLine.indexOf(pattern(nextRowIdx))) {
                allMatch = false
                loop.break()
              }
            }
            if (allMatch)
              resultFound = true
          }
        }
        if (resultFound)
          loop.break()
      }


    }
    if (resultFound)
      "YES"
    else
      "NO"
  }

  def countingSort(arr: Array[Int]): Array[Int] = {
    val max = arr.max
    val min = arr.min
    val ares = Array.ofDim[Int](max - min + 1)

    for (i <- 0 until arr.length) {
      val cur = arr(i)
      ares(cur - min) = ares(cur - min) + 1
    }
    val lb = new ListBuffer[Int]
    for (j <- 0 until ares.length) {
      val nbOccurence = ares(j)
      val v = j + min
      for (k <- 1 to nbOccurence) {
        lb.append(v)
      }
    }

    lb.toArray
  }

  // Complete the quickSort function below.
  def quickSort(arr: Array[Int]): Array[Int] = {

    val pivot = arr(0)
    val left = arr.filter(_ < pivot)
    val right = arr.filter(_ > pivot)

    println(s"${left.mkString(" ")} $pivot ${right.mkString(" ")}")
    (left :+ pivot) ++ right
  }


  def runningTime(arr: Array[Int]): Int = {
    val n = arr.length
    var count = 0
    for (currentIndex <- 0 until n) {
      for (j <- currentIndex - 1 to 0 by -1) {
        //right > left = swap
        if (arr(j + 1) < arr(j)) {
          val swap = arr(j)
          arr(j) = arr(j + 1)
          arr(j + 1) = swap
          count += 1
        }
      }
    }
    count
  }

  // Complete the insertionSort1 function below.
  def insertionSort2(n: Int, arr: Array[Int]) {
    var count = 0
    for (currentIndex <- 0 until n) {
      for (j <- currentIndex - 1 to 0 by -1) {
        //right > left = swap
        if (arr(j + 1) < arr(j)) {
          val swap = arr(j)
          arr(j) = arr(j + 1)
          arr(j + 1) = swap
          count += 1
        }
      }
    }
    println(count)
  }

  def insertionSort1(n: Int, arr: Array[Int]) {
    //    println("-------------------------------------------")
    val result = n match {
      case 0 =>
        throw new Exception("empty array")
      case 1 =>
        println(arr.mkString(" "))
      case _ =>
        //last one is not sorted
        val last = n - 1
        val unsorted = arr(last)

        //        println(s"${arr.mkString(",")} with unsorted: $unsorted")
        val antepenul = last - 1
        val loop = new Breaks
        var stop = false

        loop.breakable {
          for (i <- antepenul to 0 by -1) {
            val cur = arr(i)

            //equals
            if (i == 0 && unsorted <= cur) {
              arr(i + 1) = arr(i)
              println(arr.mkString(" "))
              arr(i) = unsorted
            }
            //
            else if (cur == unsorted) {
              arr(i + 1) = unsorted
              stop = true
            }
            //move cur to right
            else if (unsorted < cur) {
              arr(i + 1) = cur
            }
            // insert unsorted behind
            else if (unsorted > cur) {
              arr(i + 1) = unsorted
              stop = true
            }
            else {
              throw new Exception("a case is not handled")
            }

            println(arr.mkString(" "))
            if (stop)
              loop.break()
          }
        }
    }
  }
}
