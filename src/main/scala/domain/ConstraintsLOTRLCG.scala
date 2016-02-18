package domain

import util.Conf

/**
 * Created by vdoquang on 17/02/16.
 */
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

class ConstraintsLOTRLCG(id: String) extends Constraints(id) {

  override def toExport(format: String) = {
    format match {
      case "txt" => s"$id (${attributes.get("SPHERE")})"
      case "bbc" => s"$id :${attributes.getOrElse("SPHERE","N") match {case "T" => "tactique" case "R" => "connaissance" case "L" => "commandement" case "S" => "energie" case "N" => "gandalf"}}:"
    }
  }
}