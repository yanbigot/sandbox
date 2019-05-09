package com.yb

object Hierarchy {

  def main(args: Array[String]): Unit = {
    val uos = Seq(
      Map("name" -> "presidence", "id" -> "1", "parentId" -> "", "localparentid" -> "A"),
      Map("name" -> "presidence", "id" -> "1", "parentId" -> "", "localparentid" -> "A"),
      Map("name" -> "france", "id" -> "2", "parentId" -> "1", "localparentid" -> "B"),
      Map("name" -> "paris", "id" -> "3", "parentId" -> "2", "localparentid" -> "C"),
      Map("name" -> "19e", "id" -> "4", "parentId" -> "3", "localparentid" -> "D"),
      Map("name" -> "20e", "id" -> "5", "parentId" -> "3", "localparentid" -> "KO"),
      Map("name" -> "11e", "id" -> "6", "parentId" -> "3", "localparentid" -> "F")
    )

    val parentIds = uos.map(_("parentId")).distinct
    //1. find leaf
    uos.map(uo => {
      parentIds contains uo("id") match {
        case true => uo + ("isLeaf" -> "no")
        case false => uo + ("isLeaf" -> "yes")
      }
    })
      .foreach(println)
  }
}
