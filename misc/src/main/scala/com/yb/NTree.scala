package com.yb

import scala.collection.mutable.ListBuffer

case class Node[T](t: T, children: ListBuffer[Node[T]] = new ListBuffer[Node[T]]){

  def findRecursively(x: T => Boolean): Option[Node[T]] = {
    if(x.apply(t) == true)
      Some(this)
    else
      this.children.map(child => child.findRecursively(x)).headOption match {
        case Some(found) => found
        case None => None
      }
  }

  def add(child: T): Node[T] ={
    val c = Node(child)
    children.append(c)
    c
  }

}
case class Person(name: String, age: Int)

object Test{

  val parents = Map("root" -> Seq(""))
  val child   = Map("saskia" -> "root", "danielle" -> "saskia") //etc for sibling and parents

  def main(args: Array[String]): Unit = {
    val saskiaNode   = Node[Person](Person("Saskia", 7))
    saskiaNode.add(Person("Danielle",74))

    val danielleNode = saskiaNode.findRecursively(_.name == "Danielle")
    println(danielleNode.getOrElse("Not found"))

    val mark = danielleNode.get.add(Person("Mark", 5))
    val yan  = mark.add(Person("yan", 44))
    println(saskiaNode.findRecursively(_.name == "yan"))

    println(saskiaNode.findRecursively(_.name == "unknow"))
    println(saskiaNode.findRecursively(_.age == 74))
  }

}
