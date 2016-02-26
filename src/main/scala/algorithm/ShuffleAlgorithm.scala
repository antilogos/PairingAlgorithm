package algorithm

import domain.{Constraints, Subscriber, Tools}
import util.Conf

/**
 * The Shuffle algorithm will run the deterministic Greedy algorithm with different initial combinaison (just a different ordering) and keep the biggest score
 */
object ShuffleAlgorithm extends Tools {

  def arrangeSeating(initialPairing: List[List[Subscriber]], ban: List[List[List[Subscriber]]], round: Int, previousRoundPairing: List[List[List[Subscriber]]]): List[List[Subscriber]] = {
    logger(INFO,s"Score of initial pairing ${Constraints.computeRoundScore(round,initialPairing, previousRoundPairing)}")
    var i = 1;
    // We just run nth time the algorithm by just randomly changing the order of the subscriber list
    val bestOfNRun = (1 to Conf.maxTry).toList.foldLeft(initialPairing){(acc, curr) =>
      logger(TRACE, s" --- SHUFFLE ALGORITHM try $i out of ${Conf.maxTry} --- ")
      // Shuffle the ordering
      Manager.subscriberList = scala.util.Random.shuffle(Manager.subscriberList)
      val shufflePairing = scala.util.Random.shuffle(initialPairing)
      val shuffleImprovement = shufflePairing.flatten.map(sub => (sub, 0)).toMap
      val shuffleSeating = shufflePairing.flatten.map{(_, true)}
      val maxScorePairing = initialPairing
      // ----- Run the Greedy algorithm ----- //
      val tryAlgo = GreedyAlgorithm.searchSeatingForRound(shuffleImprovement, shuffleSeating, initialPairing, ban, maxScorePairing, round, previousRoundPairing)
      i = i + 1
      // Keep the better solution
      if(Constraints.computeRoundScore(round, tryAlgo, previousRoundPairing) > Constraints.computeRoundScore(round, acc, previousRoundPairing))
        tryAlgo
      else
        acc
    }
    Constraints.displayRoundReport(bestOfNRun, round, previousRoundPairing)
    bestOfNRun
  }
}