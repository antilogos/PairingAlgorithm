package util

import domain.Constraints

/**
 * Created by vdoquang on 17/02/16.
 */
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
