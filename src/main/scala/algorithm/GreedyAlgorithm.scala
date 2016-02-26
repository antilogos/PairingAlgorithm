package algorithm

import domain.{Constraints, Subscriber, Tools}
/**
 * The Greedy algorithm will start from a solution and try to improve it by replacing one subscriber until it find the local maximum.
 */
object GreedyAlgorithm extends Tools {

  /**
   *
   * @param listOfImprovement The list of subscriber that were swap if the score is > 0 and their gain
   * @param seating The list of subscriber yet seated
   * @param pairing The initial pairing
   * @param ban The ban list
   * @param maxScore The initial pairing to beat
   * @param round The number of the round
   * @param previousRound The pairing of the previous rounds
   * @return
   */
  def searchSeatingForRound(listOfImprovement: Map[Subscriber, Int], seating: List[(Subscriber, Boolean)], pairing: List[List[Subscriber]], ban: List[List[List[Subscriber]]], maxScore: List[List[Subscriber]], round: Int, previousRound: List[List[List[Subscriber]]]): List[List[Subscriber]] = {
    logger(TRACE,s" --- Greedy Algorithm --- ")
    // Take first subscriber in the non-yet-optimised list
    val pickMutation = listOfImprovement.filter(_._2 == 0).head._1
    logger(TRACE,s"Try mutation on $pickMutation")
    // Find all subscribers who could take his seating
    val constraintsAfterLeave = pairing.filter(_.contains(pickMutation)).flatten.filterNot(_.equals(pickMutation)).flatMap(_.constraints)
    val eligibleMutation = seating.map(_._1).filterNot(_.equals(pickMutation)).filter(_.constraints.intersect(constraintsAfterLeave).isEmpty)
    logger(TRACE,s"Subscribers ${eligibleMutation.map(_.id).mkString(",")} have been freed")
    // Remove all from their pairing and seating
    val newPairing = pairing.map(_.filterNot(_.equals(pickMutation)).filterNot(eligibleMutation.contains(_))).filter(_.nonEmpty)
    logger(TRACE,s"New Pairing disposition is ${newPairing.map(_.map(_.id).mkString("-")).mkString(";")} to pass to Reduce Algorithm")
    val newSeating = (pickMutation, false) :: seating.filterNot(_._1.equals(pickMutation)).filterNot(seat => eligibleMutation.contains(seat._1)) ++ eligibleMutation.map{(_, false)}
    // Add the mutation in ban list, so not to came back in initial situation
    val newBan = pairing :: ban
    // Build new compatibility map
    val newCompatibilityMap = Manager.buildCompatibilityMap()
    // ----- Run the Reduce algorithm to complete the table ----- //
    val mutatedPairing = ReduceAlgorithm.findPairing(newSeating, newCompatibilityMap, newPairing, newBan)
    // Mark mutation score as -1 if failed or the actual score
    var newImprovement = listOfImprovement
    if(mutatedPairing._1.isEmpty) {
      // Mutation did not end in a possible solution, it is marked as impossible
      logger(TRACE,s"Mutation did not result in possible pairing")
      newImprovement = newImprovement + (pickMutation -> -1)
    } else {
      // Mutation did end up in a new solution, we add it's score
      logger(TRACE,s"Mutation result in new pairing with score ${Constraints.computeRoundScore(round,mutatedPairing._1, previousRound)}")
      newImprovement = newImprovement + (pickMutation -> Constraints.computeRoundScore(round, mutatedPairing._1, previousRound))
    }
    // Keep the best solution between the new mutation and previous
    val newMaxScore = if(newImprovement.getOrElse(pickMutation,0) > listOfImprovement.values.max) {
      // We want to retry optimizing other combination if the score can be better, so we put 0 back to other mutation score if this mutation was better
      newImprovement = newImprovement.map{case (sub, score) if score < newImprovement.getOrElse(pickMutation, 0) => (sub, 0)
      case (sub, score) => (sub, score)}
      mutatedPairing._1
    } else maxScore
    logger(DEBUG, s"Current score ${newImprovement.map{case (sub, score) => s"${sub.id}:$score"}.mkString(",")}")
    // Algorithm end when we did not make any progress after trying to swap everyone
    if(newImprovement.forall(_._2 != 0)) {
      // ----- End of algorithm pass, return the best pairing ----- //
      logger(INFO,s"End of Greedy Algorithm, new score is ${newImprovement.values.max}")
      newMaxScore
    } else {
      // ----- Continue searching for the local maximum ----- //
      searchSeatingForRound(newImprovement, seating, pairing, ban, newMaxScore, round, previousRound)
    }
  }
}