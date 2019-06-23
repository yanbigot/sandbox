package com.yb

object MagicSquare {

  //rows columns and diags sums should be equals
  def formMagicSquare(arr: Array[Array[Int]]): Array[Array[Int]] = {
    //magic square formula
    //M = n(n^2+1)/2
    val n = arr.size
    val magicNumber = n * Math.pow(n, n + 1) /2

    ???
  }
}
