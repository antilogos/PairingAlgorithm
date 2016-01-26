import scala.collection.mutable

/**
 * The genetic algorithm will start from a solution and try to improve it based on a score.
 * Score are defined in this order :
 * - not part of the same group
 * - not seating with the same subscriber from previous rounds
 * - constraints are evenly distributed
 */
object GeneticAlgorithm extends Tools {
  private val numberOfTry = 1000
  private val chanceOfMutation = 0.01
  private val previousRound: scala.collection.mutable.MutableList[List[List[Subscriber]]] = mutable.MutableList()

  def arrangeSeating(initialPairing: List[List[Subscriber]]): Unit = {
    // initiale pairing : Pinit
    // calculate S, score of Pinit
    // run X times to find other P with better score
    // save Pbest as ProundY
    // make Pround- as score = 0 and Y++
    logger(INFO,s"Score of initial pairing ${computeRoundScore(0,initialPairing)}")
  }

  private def searchSeatingForRound() = {
    // Try soft mutation (A1 can be paired in P2 : A1 is marked incompatible with P1 and all that can fit in P1 are freed)
  }

  private def computeRoundScore(round: Integer, pairing: List[List[Subscriber]]) = {
    pairing.map(computeTableScore(round, _)).sum
  }

  private def computeTableScore(round: Integer, table: List[Subscriber]) = {
    var score = 100
    // Group is 50% of the weight
    if(!table.groupBy(_.group).forall(_._2.size <= 1)) {
      score = score - 50
    }
    // Already seen is 25% of the weight
    if(previousRound.exists(_.intersect(table).size > 2)) {
      score = score - 25
    }
    // Constraint repartition is 25% of the weight
    val constraintList = table.map(_.constraints)
    val sphereDiff = constraintList.flatten.groupBy(_.getKey("sphere")).mapValues(_.size)
    val sphereScore = (sphereDiff.values.max - sphereDiff.values.min) * 5
    val threatDiff = constraintList.map(_.map(_.getNum("threat")).sum)
    val threatScore = (threatDiff.max - threatDiff.min) * 4
    // Score over 250, we divide by 10 to get a 25% weight
    score = score - ( List(sphereScore + threatScore, 250).min / 10 )
    logger(INFO, s"Score for table ${table.mkString(",")}: $score")
    score
  }
}
