package com.yb

class DistinctPower {

  val aa = Range(1, 16)
  def apply(value: Range, exp: Range): Seq[Double] ={
      val result: Seq[Double] = for(v <- value; e <-exp)yield {
        Math.pow(v, e)
      }
    result.sorted.distinct
  }



}
