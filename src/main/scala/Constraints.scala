/**
 * Constraints object.
 *
 * Subscriber cannot be paired with the same constraints id.
 * Constraints have others attributes that should be evenly distributed around tables
 *
 */
class Constraints(val id: String) {

  val attributes = Constraints.inventory.getOrElse(id, Map()) + ("ID" -> id)

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

object Constraints extends Tools {

  var (fields, identityRules, calculationRules): (List[String], String, List[String]) = (Nil, "ID", Nil)

  var inventory: Map[String, Map[String, Any]] = Map()

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
    if(pair.filterNot(_.group == "").groupBy(_.group).exists(_._2.size > 1)) report += "group conflict"
    if(previousRound.exists(round => round.exists(table => table.intersect(pair).size > 1))) report += "seen in previous round"
    report += ConstraintsLOTRLCG.displayTableReport(pair.map(_.constraints).asInstanceOf[List[List[ConstraintsLOTRLCG]]])
    if(report.isEmpty) report += "ok"
    logger(3,s"Round $round - table $table - score ${computeTableScore(round,pair, previousRound)}: $report")
  }
}

object ScoreCalculationInterpreter {
  // calculationRules: List(threat=EACH VARIANCE 27 SUM $THREAT, sphere=PRODUCT GROUPBY $SPHERE)
  def compute(round: List[List[Constraints]], calculationRules: Array[String], accumulator: Int): Int = {
    if(calculationRules.length == 0) accumulator
    else calculationRules.head match {
      case arg =>
        compute(round, calculationRules.tail, accumulator)
    }
  }
}

class ConstraintsLOTRLCG(id: String) extends Constraints(id) {

  override def toExport(format: String) = {
    format match {
      case "txt" => s"$id (${attributes.get("SPHERE")})"
      case "bbc" => s"$id :${attributes.getOrElse("SPHERE","N") match {case "T" => "tactique" case "R" => "connaissance" case "L" => "commandement" case "S" => "energie" case "N" => "gandalf"}}:"
    }
  }
}

object ConstraintsLOTRLCG {
  def calculateScorePenalty(constraintList: List[List[Constraints]]): Int = {
    val sphereList = List[String]("T","R","L","S")
    // Evenly repartition of sphere is important - score from 0 to 60
    val sphereDiff = sphereList.map(sphere => constraintList.flatten.count(_.getKey("SPHERE") == sphere))
    val sphereScore = List((sphereDiff.max - sphereDiff.min) * 20 / Conf.sizeOfTable, 60).min
    // Table lacking a Sphere - bonus penalty of 40
    val lackScore = if (sphereDiff.min == 0) 40 else 0
    // Evenly repartition of threat is important - score from 0 to 20
    val threatMean = constraintList.map(_.map(_.getNum("THREAT")).sum).sum / Conf.sizeOfTable
    val threatScore = List(List(threatMean - 27, 27 - threatMean).max, 40).min
    // Score over 100
    List(sphereScore + lackScore + threatScore, 100).min
  }

  def displayTableReport(constraintList: List[List[Constraints]]) = {
    val sphereDiff = List[String]("T","R","L","S").map(sphere => (sphere, constraintList.flatten.count(_.getKey("sphere") == sphere)))
    val lackScore = sphereDiff.filter(_._2 == 0).map(_._1).mkString(",")
    s"sphere diff=$sphereDiff - lack: $lackScore"
  }
}