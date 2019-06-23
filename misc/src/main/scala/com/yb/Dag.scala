package com.yb

import scala.collection.mutable

object Dag {

  case class Verticle(id: Int, v: String)

  val verticles = mutable.Map.empty[Int, Verticle]
  val edges     = mutable.Map.empty[Int, Int]

  def add(verticle: Verticle, optOfEdges: Option[Seq[Int]]) = {
    verticles.put(verticle.id, verticle)

    optOfEdges match {
      case Some(edgeList) => edgeList.foreach(e => {
        edges.put(verticle.id, e)
      })
    }
  }

  def shortestPath(from: Int, to: Int): Unit ={
    val stack = mutable.Stack.empty


    def inner(from: Int, to: Int, stack: mutable.Stack[Int]): mutable.Stack[Int] ={
      ???
    }

  }
}
