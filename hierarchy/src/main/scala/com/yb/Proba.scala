package com.yb

import com.yb.Converter.Translater
object Proba {

  case class Event(me: Double, p2: Double, p3: Double)

  case class Result(bet: Double, win: Boolean)

  case class BetAndNbGoodOutcome(goodOutcome: Double, bet: Double)

  standardEvents
  val standardEvents = Seq(
    Event(1, 1, 1), //111
    Event(1, 1, -1), //110
    Event(1, -1, 1), //101
    Event(1, -1, -1), //100
    Event(-1, 1, 1), //011
    Event(-1, 1, -1), //010
    Event(-1, -1, 1), //001
    Event(-1, -1, -1) //000
  )
  val enforceBetEvents = Seq(
    Event(2, 2, 2), //111
    Event(2, 2, -1), //110
    Event(2, -1, 2), //101
    Event(2, -1, -1), //100
    Event(-1, 2, 2), //011
    Event(-1, 2, -1), //010
    Event(-1, -1, 2), //001
    Event(-1, -1, -1) //000
  )

  def main(args: Array[String]): Unit = {
    val me = 100.0
    val p2 = 100.0
    val p3 = 100.0
    println {
      s"me $me vs p2 $p2 vs p3 $p3 \n" +
        blindBestBet(me, p2,p3)
        bestWageWithKnownBets(me, p2, p3, 75.0, 25.0)
    }
  }

  def blindBestBet(myScore: Double, score2: Double, score3: Double): Double = {
    //analyse scores
    val computedEvents: Seq[Result] = for (
      myBet: Double <- 0.0 to myScore by 1;
      p2Bet: Double <- 0.0 to score2 by 1;
      p3Bet: Double <- 0.0 to score3 by 1;
      event <- standardEvents
    ) yield {

      val myUpdateScore = Double2double(myBet * event.me + myScore)
      val p2UpdateScore = Double2double(p2Bet * event.p2 + score2)
      val p3UpdateScore = Double2double(p3Bet * event.p3 + score3)

      val r: Result = if ((myUpdateScore > p2UpdateScore) && (myUpdateScore > p3UpdateScore))
        Result(myBet, true)
      else
        Result(myBet, false)

      /**
        * println(
        * s"""[${
        *event.me match {
        * case 2.0 => "Win";
        * case _ => "Loose"
        * }
        * }] bet = $myBet, total = $myUpdateScore, score= $myScore
        * |[${
        *event.p2 match {
        * case 2.0 => "Win";
        * case _ => "Loose"
        * }
        * }] bet = $p2Bet, total = $p2UpdateScore, score= $score2
        * |[${
        *event.p3 match {
        * case 2.0 => "Win";
        * case _ => "Loose"
        * }
        * }] bet = $p3Bet, total = $p2UpdateScore, score= $score3
        * |$r
        * """.stripMargin)
        */
      r
    }

    val g: Map[Double, Seq[Result]] = computedEvents
      .groupBy(x => x.bet)


    g.map(x => (x._1, x._2.count(_.win == true).toDouble, x._2.size.toDouble)).toSeq.sortBy(_._1).foreach(x => println(s"with ${x._1} ${x._2} out of ${x._3}"))

    val all = g
      .map { b =>
        val goodOutcome = b._2.count(_.win == true).toDouble / b._2.size.toDouble
        BetAndNbGoodOutcome(goodOutcome, b._1)
      }
      .toSeq

    all.sortBy(_.goodOutcome).sortBy(_.bet).foreach(println)

    val max = all.maxBy(_.goodOutcome)
    println(s"max $max")
    val best = all.filter(_.goodOutcome == max.goodOutcome).minBy(_.bet)


    best.bet
  }
  def bestWageWithKnownBets(myScore: Double, score2: Double, score3: Double, p2Bet: Double, p3Bet: Double): Double = {
    //analyse scores
    val computedEvents: Seq[Result] = for (
      myBet: Double <- 0.0 to myScore by 1;
      event <- standardEvents
    ) yield {

      val myUpdateScore = Double2double(myBet * event.me + myScore)
      val p2UpdateScore = Double2double(p2Bet * event.p2 + score2)
      val p3UpdateScore = Double2double(p3Bet * event.p3 + score3)

      val r: Result = if ((myUpdateScore > p2UpdateScore) && (myUpdateScore > p3UpdateScore))
        Result(myBet, true)
      else
        Result(myBet, false)


       println(
       s"""[${
      event.me.translate
       }] bet = $myBet, total = $myUpdateScore, score= $myScore
       |[${
      event.p2.translate
       }] bet = $p2Bet, total = $p2UpdateScore, score= $score2
       |[${
      event.p3.translate
       }] bet = $p3Bet, total = $p2UpdateScore, score= $score3
       |$r
       """.stripMargin)

      r
    }

    val g: Map[Double, Seq[Result]] = computedEvents
      .groupBy(x => x.bet)


    g.map(x => (x._1, x._2.count(_.win == true).toDouble, x._2.size.toDouble)).toSeq.sortBy(_._1).foreach(x => println(s"with ${x._1} ${x._2} out of ${x._3}"))

    val all = g
      .map { b =>
        val goodOutcome = b._2.count(_.win == true).toDouble / b._2.size.toDouble
        //        println(s"goodOutcome $goodOutcome")
        BetAndNbGoodOutcome(goodOutcome, b._1)
      }
      .toSeq

    all.sortBy(_.goodOutcome).sortBy(_.bet).foreach(println)

    val max = all.maxBy(_.goodOutcome)
    println(s"max $max")
    //    all.filter(_._1 == max).foreach(println)
    val best = all.filter(_.goodOutcome == max.goodOutcome).minBy(_.bet)


    best.bet
  }

}
object Converter{
  implicit class Translater(val i: Double){
    def translate = i match {
      case 1 => "Win"
      case -1 => "Loose"
    }
  }
}
