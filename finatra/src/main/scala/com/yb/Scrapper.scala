package com.yb

import com.twitter.finagle.http.Request
import com.twitter.finatra.http.routing.HttpRouter
import com.twitter.finatra.http.{Controller, HttpServer}
import scalaj.http.{Http, HttpOptions, HttpRequest, HttpResponse}


object Scrapper extends ScrapperServer

class ScrapperServer extends HttpServer {
  override protected def configureHttp(router: HttpRouter): Unit = {
    router.add[Scrapper]
  }

  override val defaultHttpPort: String = ":8899"
}
class Scrapper extends Controller{

  val tables = (for(i <- 1 to 2) yield {s"table-$i"}).toSeq :+ "table-1a"

  get("/scrap"){
    request: Request => {
      println("scrapping...")
      tables.foreach(println)
      for(table <- tables) yield{
        val url = s"https://ucr.fbi.gov/crime-in-the-u.s/2016/crime-in-the-u.s.-2016/tables/$table/${table}.xls/output.xls"
        println(s" - $url")
        val request: HttpRequest = Http(url).option(HttpOptions.allowUnsafeSSL)
        import Excel2Csv.toCsvFormatter
        val xls = request.asString.body.toCsv
        import java.nio.file.{Paths, Files}
        import java.nio.charset.StandardCharsets

        Files.write(Paths.get(s"$table.xls"), xls.getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
