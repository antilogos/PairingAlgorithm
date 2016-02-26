package domain

import util.{ScoreCalculationInterpreter, Conf}

import scala.util.{Failure, Success, Try}

/**
 * Created by vdoquang on 17/02/16.
 */
object Constraints extends Tools {

  var (fields, identityRules, calculationRules): (List[String], String, List[String]) = (Nil, "ID", Nil)

  /* Init with blank user */
  var inventory: Map[String, Map[String, Any]] = Map("" -> Map("ID" -> ""))

  val blank = new Constraints("")

  def calculateScorePenalty(constraintList: List[List[Constraints]]): Int = {
    calculationRules.map{_.split("=")}
      .map{rule => ScoreCalculationInterpreter.compute(constraintList, rule(1).split(" "), 0)}
    .sum
  }

  def displayRoundReport(pairing: List[List[Subscriber]], round: Int, previousRound: List[List[List[Subscriber]]]) = {
    (1 to pairing.size).toList.foreach{i => displayTableReport(round, i, pairing(i-1), previousRound)}
  }

  def computeRoundScore(round: Integer, pairing: List[List[Subscriber]], previousRound: List[List[List[Subscriber]]]): Int = {
    pairing.map(computeTableScore(round, _, previousRound)).sum
  }

  def computeTableScore(round: Integer, pair: List[Subscriber], previousRound: List[List[List[Subscriber]]]) = {
    var score = Conf.groupScoreWeight + Conf.alreadySeenScoreWeight + Conf.constrainstSpecificScoreWeight
    // Group is 50% of the weight
    if(pair.filterNot(_.group == "").groupBy(_.group).exists(_._2.size > 1)) {
      score = score - Conf.groupScoreWeight
    }
    // Already seen is 25% of the weight
    if(previousRound.exists(round => round.exists(table => table.intersect(pair).size > 1))) {
      score = score - Conf.alreadySeenScoreWeight
    }
    // Constraint repartition is 25% of the weight
    val constraintList = pair.map(_.constraints)
    score = score - ( ConstraintsLOTRLCG.calculateScorePenalty(constraintList.asInstanceOf[List[List[ConstraintsLOTRLCG]]]) * Conf.constrainstSpecificScoreWeight / 100)
    logger(TRACE, s"Score for table ${pair.mkString(",")}: $score")
    score
  }

  def displayTableReport(round: Integer, table: Int, pair: List[Subscriber], previousRound: List[List[List[Subscriber]]]) = {
    var report = ""
    if(pair.filterNot(_.group == "").groupBy(_.group).exists(_._2.size > 1)) report += " group conflict "
    if(previousRound.exists(round => round.exists(table => table.intersect(pair).size > 1))) report += " seen in previous round : " + previousRound.map(round => round.filter(table => table.intersect(pair).size > 1)).flatMap(round => round.flatMap(table => table.map(sub => sub.id))).mkString(":")
    report += ConstraintsLOTRLCG.displayTableReport(pair.map(_.constraints).asInstanceOf[List[List[ConstraintsLOTRLCG]]])
    if(report.isEmpty) report += "ok"
    logger(INFO,s"Round $round - table $table - score ${computeTableScore(round,pair, previousRound)}: $report")
  }
}

/**
 * Constraints object.
 *
 * domain.Subscriber cannot be paired with the same constraints id.
 * Constraints have others attributes that should be evenly distributed around tables
 *
 */
class Constraints(val id: String) extends Tools {

  val attributes = Constraints.inventory.get(id) match {
    case Some(value) => value + ("ID" -> id)
    case None => logger(INFO, s"No key $id found"); Map("ID" -> id)
  }

  def getNum(key: String): Int = {
    attributes.get(key.toUpperCase).get.toString.toInt
  }

  def getKey(key: String): String = attributes.get(key.toUpperCase).get.toString

  def toExport(format: String) = {
    s"$id"
  }
  override def toString = {
    s"$id"
  }
}