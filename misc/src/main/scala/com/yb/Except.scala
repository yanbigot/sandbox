package com.yb

import scala.util.Try
import scala.util.control.Exception._

object Except {

  def main(args: Array[String]): Unit = {

    def doIt(x: String, i: Int) = s"*${x.toInt / i}*"

    val x = allCatch opt doIt("x", 1)
    println(x)

    println {
      Try("x".toInt).toOption
    }


  }
}
