package com.yb
class Zeus
class ExtracThor(val s: String, val i: Int) extends Zeus {
}
object ExtracThor{
  def apply(s: String, i: Int): Unit ={
    new ExtracThor(s,i)
  }

  def unapply(e: ExtracThor): Option[(String, Int)] = Some(e.s,e.i)

  def main(args: Array[String]): Unit = {
    val e = new ExtracThor("s", 1)

    e match {
      case t: ExtracThor => println("T'as pas tort")
      case ExtracThor(s: String,_) => println("Thor")
      case z: Zeus => println("God")
      case _ => println("failed")
    }
  }
}
