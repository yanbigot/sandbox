package com.yb

import com.opentable.db.postgres.embedded.EmbeddedPostgres

import scala.collection.mutable.ListBuffer
//TODO find a way to restore a dump ...
object EmbeddedPostgresTest {
  private[this] val pg = EmbeddedPostgres.start

  case class Column(idx: Int, name: String, dType: Int)

  private[this] val BIT = -7
  private[this] val TINYINT = -6
  private[this] val BIGINT = -5
  private[this] val LONGVARBINARY = -4
  private[this] val VARBINARY = -3
  private[this] val BINARY = -2
  private[this] val LONGVARCHAR = -1
  private[this] val NULL = 0
  private[this] val CHAR = 1
  private[this] val NUMERIC = 2
  private[this] val DECIMAL = 3
  private[this] val INTEGER = 4
  private[this] val SMALLINT = 5
  private[this] val FLOAT = 6
  private[this] val REAL = 7
  private[this] val DOUBLE = 8
  private[this] val VARCHAR = 12
  private[this] val DATE = 91
  private[this] val TIME = 92
  private[this] val TIMESTAMP = 93
  private[this] val OTHER = 1111

  def withConnection(q: String): Unit = {
    try {
      val c = pg.getPostgresDatabase.getConnection
      try {
        val s = c.createStatement
        val rs = s.executeQuery(q)
        val metadata = rs.getMetaData
        val columns = for (cIdx <- 0 until metadata.getColumnCount) yield {
          Column(cIdx, metadata.getColumnName(cIdx), metadata.getColumnType(cIdx))
        }
        var rows = new ListBuffer[Map[String, String]]
        while (rs.next()) {
          val row = for (col <- columns) yield {
            col.dType match {
              case VARCHAR =>
                (col.name, rs.getString(col.name))
              case _ =>
                (col.name, rs.getObject(col.name).toString)
            }
          }
          rows += (row.toMap)
        }
      } catch {
        case e: Exception =>
          System.out.print(e)
      } finally {
        if (pg != null) pg.close()
        if (c != null) c.close()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    testConnection()
    val x = withConnection("SELECT 2")
    println(x)
  }

  def testConnection() = {
    try {
      val pg = EmbeddedPostgres.start
      val c = pg.getPostgresDatabase.getConnection
      try {
        val s = c.createStatement
        val rs = s.executeQuery("SELECT 1")
        assert(rs.next)
        assert(rs.getInt(1) == 1)
        //        assert(rs.next)
      } catch {
        case e: Exception =>
          System.out.print(e)
      } finally {
        if (pg != null) pg.close()
        if (c != null) c.close()
      }
    }
  }
}
