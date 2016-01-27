import scala.collection.mutable

/**
 * The genetic algorithm will start from a solution and try to improve it based on a score.
 * Score are defined in this order :
 * - not part of the same group
 * - not seating with the same subscriber from previous rounds
 * - constraints are evenly distributed
 */
object GeneticAlgorithm extends Tools {
  //private val numberOfTry = 1000
  //private val chanceOfMutation = 0.01
  val previousRound: scala.collection.mutable.MutableList[List[List[Subscriber]]] = mutable.MutableList()
  val groupScoreWeight = Main.configuration.getInt("conf.score.group")
  val alreadySeenScoreWeight = Main.configuration.getInt("conf.score.alreadySeen")
  val constrainstSpecificScoreWeight = Main.configuration.getInt("conf.score.constraintSpecific")

  def arrangeSeating(initialPairing: List[List[Subscriber]], ban: List[List[List[Subscriber]]], round: Int): List[List[Subscriber]] = {
    // initiale pairing : Pinit
    // calculate S, score of Pinit
    // run X times to find other P with better score
    // save Pbest as ProundY
    // make Pround- as score = 0 and Y++
    logger(INFO,s"Score of initial pairing ${computeRoundScore(round,initialPairing)}")
    searchSeatingForRound(initialPairing.flatten.map{(_, 0)}.toMap, initialPairing.flatten.map{(_, true)}, initialPairing, ban, initialPairing, round)
  }

  private def searchSeatingForRound(listOfImprovement: Map[Subscriber, Int], seating: List[(Subscriber, Boolean)], pairing: List[List[Subscriber]], ban: List[List[List[Subscriber]]], maxScore: List[List[Subscriber]], round: Int): List[List[Subscriber]] = {
    // Try soft mutation (A1 can be paired in P2 : A1 is marked incompatible with P1 and all that can fit in P1 are freed)

    // Take first subscriber
    val pickMutation = listOfImprovement.filter(_._2 == 0).head._1
    logger(TRACE,s"Try mutation on $pickMutation")
    // Find all subscribers who could take his seating
    val constraintsVoid = pairing.filter(_.contains(pickMutation)).flatten.filterNot(_.equals(pickMutation)).flatMap(_.constraints)
    val eligibleMutation = seating.map(_._1).filterNot(_.equals(pickMutation)).filter(_.constraints.intersect(constraintsVoid).isEmpty)
    logger(TRACE,s"Subscribers ${eligibleMutation.map(_.id).mkString(",")} have been freed")
    // Remove all from their pairing
    val newPairing = pairing.map(_.filterNot(_.equals(pickMutation)).filterNot(eligibleMutation.contains(_))).filter(_.nonEmpty)
    logger(TRACE,s"New Pairing disposition is ${newPairing.map(_.map(_.id).mkString("-")).mkString(";")} to pass to Pairing Algorithm")
    val newSeating = (pickMutation, false) :: seating.filterNot(_.equals(pickMutation)).filterNot(eligibleMutation.contains(_)) ++ eligibleMutation.map{(_, false)}
    // Add the mutation in ban list
    val newBan = pairing :: ban
    val mutatedPairing = PairingAlgorithm.findPairing(newSeating, PairingAlgorithm.buildCompatibilityMap(seating.map(_._1)), newPairing, newBan)
    var newImprovement = listOfImprovement
    if(mutatedPairing._1.isEmpty) {
      logger(TRACE,s"Mutation did not result in possible pairing")
      newImprovement = newImprovement + (pickMutation -> -1)
    } else {
      logger(TRACE,s"Mutation result in new pairing with score ${computeRoundScore(round,mutatedPairing._1)}")
      newImprovement = newImprovement + (pickMutation -> computeRoundScore(round, mutatedPairing._1))
    }
    // Add the score to the list
    val newMaxScore = if(listOfImprovement.forall(newImprovement.getOrElse(pickMutation,0) > _._2)) {
      // We want to retry optimizing other combination if the score can be better
      newImprovement = newImprovement.map{case (sub, score) if score > 0 && score < newImprovement.getOrElse(pickMutation, 0) => (sub, 0)
      case (sub, score) => (sub, score)}
      mutatedPairing._1
    } else maxScore
    logger(DEBUG, s"Current score ${newImprovement.map{case (sub, score) => s"${sub.id}:$score"}.mkString(",")}")
    if(newImprovement.forall(_._2 != 0)) {
      // End of algorithm pass, return the best pairing
      logger(INFO,s"End of Genetic Algorithm, new score is ${listOfImprovement.valuesIterator.max}")
      // Add the situation in previous round
      previousRound += newMaxScore
      logger(TRACE,s"Pairing for round $round is:\n${newMaxScore.map(table => table.mkString("\t")).mkString("\n")}")
      newMaxScore
    } else {
      searchSeatingForRound(newImprovement, seating, pairing, ban, newMaxScore, round)
    }
  }

  private def computeRoundScore(round: Integer, pairing: List[List[Subscriber]]): Int = {
    pairing.map(computeTableScore(round, _)).sum
  }

  private def computeTableScore(round: Integer, pair: List[Subscriber]) = {
    var score = groupScoreWeight + alreadySeenScoreWeight + constrainstSpecificScoreWeight
    // Group is 50% of the weight
    if(pair.filterNot(_.group == "").groupBy(_.group).exists(_._2.size > 1)) {
      score = score - groupScoreWeight
    }
    // Already seen is 25% of the weight
    if(previousRound.exists(round => round.exists(table => table.intersect(pair).size > 1))) {
      score = score - alreadySeenScoreWeight
    }
    // Constraint repartition is 25% of the weight
    val constraintList = pair.map(_.constraints)
    score = score - ( ConstraintsLOTRLCG.calculateScorePenalty(constraintList.asInstanceOf[List[List[ConstraintsLOTRLCG]]]) * constrainstSpecificScoreWeight / 100)
    logger(TRACE, s"Score for table ${pair.mkString(",")}: $score")
    score
  }
}