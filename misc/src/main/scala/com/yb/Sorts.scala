package com.yb

object Sorts {

  val array = Array(9, 7, 1, 3, 8, 5)
  val expected = array.sorted

  def main(args: Array[String]): Unit = {
    //    val bubble = bubbleSort(array)
    //    println(bubble.mkString(","))

//    val arrs: Array[Int] = Range(100000, 200000).toArray ++ Range(1, 50000).toArray

    val list = List.empty[Int]
    val sB = sortB(list.toArray)
    println(sB.mkString(","))
  }

  def bubbleSort(a: Array[Int]): Array[Int] = {
    var swap = false
    val last = a.length - 2
    for (position <- 0 to last) {
      val cur = a(position)
      val next = a(position + 1)

      if (cur > next) {
        a(position) = next
        a(position + 1) = cur
        println(s"swap ${a.mkString(",")}")
        swap = true
      }
    }
    if (swap)
      bubbleSort(a)
    else
      a
  }

  def sortB(a: Array[Int]): Array[Int] = {
    val last = a.length - 2


    //iterate on all
    for (position <- 0 to last) {
      val cur = a(position)
      println(s" cur: $cur")
      var nextPosition = position + 1
      var next = a(nextPosition)

      while (nextPosition <= last + 1 && cur > a(nextPosition)) {
        //save the compared value
        next = a(nextPosition)
        println(s" next: $next")
        //swap
        a(nextPosition) = cur
        a(nextPosition - 1) = next

        //inc
        nextPosition = nextPosition + 1
      }
//      println(s" new one: ${a.mkString(",")}")

    }
    a
  }
}
