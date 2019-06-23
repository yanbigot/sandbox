package com.yb


import scala.util.control._
import scala.util.{Failure, Success, Try}

object HackerRank_01 {

  def main(args: Array[String]): Unit = {
    //    val diffDiag = diagonalDifference(
    //      Array(
    //        Array(1,3,9),
    //        Array(100,1,9),
    //        Array(1,30,1)
    //      )
    //    )
    //    println(diffDiag)

    //    plusMinus(
    //      Array(-4, 3, -9, 0, 4, 1)
    //    )

    //    staircase(80)

    //    miniMaxSum(Array(-1,-2,-3,-10))

    //    birthdayCakeCandles(Array(33, 6, 9, 33))

    //    println(timeConversion("07:05:45PM"))
    //    println(timeConversion("12:05:45PM"))

    //    val k = kangaroo(0, 3, 4, 2)
    //    println(k)

    //    gradingStudents(Array(73, 67, 38, 33)).foreach(println)
    //    println(3%5)
    //    println(10%11)
    //    println{
    //      saveThePrisoner(7,19,2)
    //    }

    //    assert(saveThePrisoner(7, 19, 2) == 6)
    //    assert(saveThePrisoner(3, 7, 3) == 3)
    //    assert(saveThePrisoner(5, 2, 2) == 3)
    //    assert(saveThePrisoner(5, 2, 1) == 2)
    //    assert(saveThePrisoner(5, 5, 1) == 1)
    //    assert(saveThePrisoner(5, 5, 5) == 5)

    //    circularArrayRotation(Array(1, 2, 3,4, 5 , 6, 7), 2, Array(1))
    //    println(angryProfessor(3, Array(-1, 1, 1, -3)))

    //    assert(encryption("haveaniceday") == "hae and via ecy")
    //    assert(encryption("feedthedog") == "fto ehg ee dd")
    //    assert(encryption("chillout") == "clu hlt io")

    "abcdefghijklmnopqrstuvwxyz".toCharArray.map(_.toInt).foreach(println)
  }

  def biggerIsGreater(w: String): String = {
    val inted = w.toCharArray.map(_.toInt)
    var success = false
    val loop = new Breaks
    var result = w.toCharArray

    loop.breakable {
      for (i <- inted.length - 1 until 0 by -1) {
        val cur = inted(i)
        val pre = inted(i - 1)
        if (cur > pre) {
          val swap = result(i)
          result(i) = result(i - 1)
          result(i - 1) = swap
          success = true
          loop.break()
        }
      }
    }

    if(success)
      result.mkString
    else
      "no answer"
  }

  // Complete the encryption function below.
  def encryption(s: String): String = {
    //remove spaces
    val noSpace = s.replaceAll(" ", "")
    val len = noSpace.size
    val sq = Math.sqrt(len)
    var rows = Math.floor(sq) toInt
    var cols = rows + 1 toInt

    if (rows * cols < len)
      rows += 1

    val matrix = Array.ofDim[Char](rows, cols)

    var charPosition = 0
    println(
      s"""noSpace: $noSpace
         |len: $len
         |sq: $sq
         |rows: $rows
         |cols: $cols
       """.stripMargin)
    for (row <- 0 until rows;
         col <- 0 until cols) {
      Try(noSpace(charPosition)) match {
        case Success(value) => matrix(row)(col) = value
        case Failure(e) =>
      }
      charPosition += 1
    }

    println(
      s"""
         |mat: ${matrix.map(_.mkString).mkString("\n---\n", "\n", "\n---")}
         |mat: rows: ${matrix.size}, cols:${matrix.head.size}
       """.stripMargin)

    val result = new StringBuffer()
    for (
      c <- 0 until cols;
      r <- 0 until rows
    ) {
      val w: Char = matrix(r)(c)
      if ('\u0000' != w)
        result.append(w)
      else
        println(s" matrix($r)($c) is null")

      val last = r == rows - 1
      if (last)
        result.append(' ')
    }
    println(s"[${result.toString.trim}]")
    result.toString.trim
  }

  def utopianTree(n: Int): Int = {
    val cycles = n + 1
    var res = 0
    for (cycle <- 1 to cycles) {
      if (cycle % 2 == 1)
        res = res + 1
      else
        res = res * 2
    }

    res
  }

  // Complete the angryProfessor function below.
  def angryProfessor(k: Int, a: Array[Int]): String = {
    //k threesold
    a.count(_ <= 0) >= k match {
      case true => "NO"
      case false => "YES"
    }
  }

  // Complete the catAndMouse function below.
  def catAndMouse(x: Int, y: Int, z: Int): String = {
    if (Math.abs(x - z) < Math.abs(y - z))
      "Cat A"
    else if (Math.abs(x - z) > Math.abs(y - z))
      "Cat B"
    else
      "Mouse C"

  }

  // Complete the permutationEquation function below.
  def permutationEquation(p: Array[Int]): Array[Int] = {
    //find the index of the given integer

    for (index <- 0 until p.length) yield {
      val value = p(index) // + 1
      val indexOfPermValue = p find (_ == value) get

    }
    ???
  }

  // Complete the circularArrayRotation function below.
  def circularArrayRotation(a: Array[Int], k: Int, queries: Array[Int]): Array[Int] = {

    println(a.mkString(","))
    val lastIndex = a.length - 1
    println(s"lastIndex: $lastIndex")

    for (i <- 1 to k) {
      val firstVal = a(lastIndex)
      println(s"firstVal: $firstVal")
      for (j <- lastIndex to 1 by -1) {
        a(j) = a(j - 1)
      }
      a(0) = firstVal
    }
    println(a.mkString(","))
    queries.map(qIdex => a(qIdex))
  }

  // Complete the saveThePrisoner function below.
  def saveThePrisoner(n: Int, m: Int, s: Int): Int = {

    val result = if (s + m < n) {
      s + m - 1
    }
    else if ((s + m - 1) % n == 0) {
      n
    }
    else {
      ((s + m - 1) % n)
    }
    result
  }

  def gradingStudents(grades: Array[Int]): Array[Int] = {
    // Write your code here
    def roundGrade(grade: Int): Int = {
      val asChars = grade.toString
      val unit = asChars.takeRight(1).toInt


      unit match {
        case 3 => grade - unit + 5
        case 4 => grade - unit + 5
        case 8 => grade - unit + 10
        case 9 => grade - unit + 10
        case _ => grade
      }
    }

    val res: Array[Int] = for (grade <- grades) yield {
      if (grade < 38)
        grade
      else
        roundGrade(grade)
    }
    res
  }

  // Complete the kangaroo function below.
  def kangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
    val startX = x1
    val startY = x2
    val jumpX = v1
    val jumpY = v2

    var result = "NO"

    val loop = new Breaks


    loop.breakable {

      for (n <- 0 to 10000) {
        val kangarooX = startX + n * jumpX
        val kangarooY = startY + n * jumpY

        if (kangarooX == kangarooY) {
          result = "YES"
          loop.break()
        }
      }
    }

    result
  }

  // Complete the compareTriplets function below.
  def compareTriplets(a: Array[Int], b: Array[Int]): Array[Int] = {
    val size = a.size

    val scores: Seq[(Int, Int)] = for (i <- 0 to size) yield {
      if (a(i) > b(i))
        (1, 0)
      else if (a(i) < b(i))
        (0, 1)
      else
        (0, 0)
    }
    val r = scores.reduce(
      (result, score)
      => (result._1 + score._1, result._2 + score._2))

    Array(r._1, r._2)

  }

  // Complete the aVeryBigSum function below.
  def aVeryBigSum(ar: Array[Long]): Long = {
    ar.reduce(_.toLong + _.toLong)
  }

  /*
 * Complete the 'diagonalDifference' function below.
 *
 * The function is expected to return an INTEGER.
 * The function accepts 2D_INTEGER_ARRAY arr as parameter.
 */

  def diagonalDifference(arr: Array[Array[Int]]): Int = {
    val diag1 = for (iRow <- 0 until arr.length)
      yield arr(iRow)(iRow)
    val diag2: Seq[Int] = for (iRow <- 0 until arr.length)
      yield arr(iRow)(arr.size - 1 - iRow)
    Math.abs(diag1.reduce(_ + _) - diag2.reduce(_ + _))
  }

  // Complete the plusMinus function below.
  def plusMinus(arr: Array[Int]): Unit = {
    val size = arr.length

    val plus = arr.count(_ > 0).toFloat / size.toFloat
    val mins = arr.count(_ < 0).toFloat / size.toFloat
    val zers = arr.count(_ == 0).toFloat / size.toFloat

    println(arr.count(_ > 0).toFloat / size.toFloat)
    println(arr.count(_ < 0).toFloat / size.toFloat)
    println(arr.count(_ == 0).toFloat / size.toFloat)

  }

  // Complete the staircase function below.
  def staircase(n: Int) {
    for (row <- 1 to n) {
      val spaces = (1 to n - row).map(x => " ").mkString
      val dashes = (1 to row).map(x => "#").mkString

      println(s"$spaces$dashes")
    }
  }

  // Complete the miniMaxSum function below.
  def miniMaxSum(arr: Array[Int]) {
    var min = arr(0)
    var max = arr(0)
    var minIdx = 0
    var maxIdx = 0

    for (i <- 0 until arr.length) yield {
      if (arr(i) <= min) {
        minIdx = i
        min = arr(i)
      }
      if (arr(i) >= max) {
        maxIdx = i
        max = arr(i)
      }

    }

    val minSum = arr.zipWithIndex.filterNot(_._2 == maxIdx).map(_._1.toLong).sum
    val maxSum = arr.zipWithIndex.filterNot(_._2 == minIdx).map(_._1.toLong).sum

    println(s"$minSum $maxSum")
  }

  def birthdayCakeCandles(ar: Array[Int]): Int = {
    val max = ar.max

    ar.count(_ == max)

  }

  /*
  * Complete the timeConversion function below.
  */
  def timeConversion(s: String): String = {

    val pattern = "([0-9]+):([0-9]+):([0-9]+)(.*)".r
    val pattern(hours, minutes, secondes, pam) = s

    pam match {
      case "AM" if (hours.toInt == 12) =>
        s"00:$minutes:$secondes"
      case "AM" if (hours.toInt != 12) =>
        s"$hours:$minutes:$secondes"

      case "PM" if (hours.toInt == 12) =>
        s"$hours:$minutes:$secondes"
      case "PM" =>
        s"${hours.toInt + 12}:$minutes:$secondes"
    }
  }
}
