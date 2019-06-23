package com.yb

import java.io.{File, FileOutputStream, PrintStream}

import org.apache.poi.sl.usermodel.Sheet
import org.apache.poi.ss.usermodel.DataFormatter
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.apache.poi.ss.usermodel.Cell
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.usermodel.Row
import scala.collection.JavaConversions._

object Excel2Csv {

  implicit class toCsvFormatter(val xlsxFile: String) {

    def toCsv: String = {
      val csvFile = xlsxFile.replaceAll("xls", "csv")
      val wb = new XSSFWorkbook(new File(xlsxFile))
      val formatter: DataFormatter = new DataFormatter();

      val out: PrintStream = new PrintStream(new FileOutputStream(csvFile),
        true, "UTF-8")

      val asString = new StringBuilder
      val bom = Array[Byte](0xEF.toByte, 0xBB.toByte, 0xBF.toByte)
      out.write(bom)

      for (sheet <- wb) {
        for (row <- sheet) {
          var firstCell = true
          for (cell <- row) {
            if (!firstCell) out.print(',')
            val text = formatter.formatCellValue(cell)
            asString.append(text)
//            out.print(text)
            firstCell = false
          }
          asString.append("\n")
//          out.println
        }
      }
      asString.toString()
    }
  }

}
