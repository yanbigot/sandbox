package yb

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.gatling.core.json.Jackson
import java.nio.charset.StandardCharsets.UTF_8

import io.gatling.core.config.GatlingConfiguration

import scala.concurrent.duration._

object BDDSimulation{
  def main(args: Array[String]): Unit = {
    new BDDSimulation()
  }
}

class BDDSimulation extends Simulation {

  val httpProtocol = http
    .baseUrl("http://localhost:8888") // Here is the root for all relative URLs
    .acceptHeader("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8") // Here are the common headers
    .doNotTrackHeader("1")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .acceptEncodingHeader("gzip, deflate")
    .userAgentHeader("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:16.0) Gecko/20100101 Firefox/16.0")

  val headers_10 = Map("Content-Type" -> "application/x-www-form-urlencoded") // Note the headers specific to a given request

//  implicit configuration: GatlingConfiguration

  lazy val notOnlyPRGMAsJson = Jackson().parse(notOnlyPRGM)

  val notOnlyPRGM =
    """{
      |"emp":{
      | "flw": [
      |   {"typeCode": "PRGM"},
      |   {"typeCode": "PRGM"},
      |   {"typeCode": "PRGM"},
      |   {"typeCode": "EVNT"}
      | ]
      |}
      |}""".stripMargin

  val scn = scenario("Scenario Name") // A scenario is a chain of requests and pauses
    .exec(http("1 notOnlyPRGM")
    .get("/notOnlyPRGM")
      .check(jsonPath("$..typeCode").findAll.transform((s: Seq[String]) => s.count(_ != "PRGM")).gte(0)
  )).exec(http("2 onlyPRGM")
    .get("/onlyPRGM")
      .check(jsonPath("$..typeCode").findAll.transform((s: Seq[String]) => s.count(_ != "PRGM")).gte(0)
  )).exec(http("3 notOnlyPRGM")
    .get("/onlyPRGM")
      .check(bodyString.is(notOnlyPRGM)
//      .check(bodyString.find.is(notOnlyPRGMAsJson)
  ))





  setUp(scn.inject(atOnceUsers(1)).protocols(httpProtocol))
}
