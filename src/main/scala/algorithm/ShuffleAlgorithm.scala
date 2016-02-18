package algorithm

import domain.{Constraints, Subscriber, Tools}

/**
 * The Shuffle algorithm will run the deterministic Greedy algorithm with different initial combinaison (just a different ordering) and keep the biggest score
 */
object ShuffleAlgorithm extends Tools {

  val maxTry = 10

  def arrangeSeating(initialPairing: List[List[Subscriber]], ban: List[List[List[Subscriber]]], round: Int, previousRound: List[List[List[Subscriber]]]): List[List[Subscriber]] = {
    logger(INFO,s"Score of initial pairing ${Constraints.computeRoundScore(round,initialPairing, previousRound)}")
    var i = 1;
    val bestOfNRun = (1 to maxTry).toList.foldLeft(initialPairing){(acc, curr) =>
      logger(TRACE, s" --- SHUFFLE ALGORITHM try $i out of $maxTry --- ")
      // Shuffle the ordering
      val shufflePairing = scala.util.Random.shuffle(initialPairing)
      // Run the Greedy algorithm
      val tryAlgo = GreedyAlgorithm.searchSeatingForRound(shufflePairing.flatten.map{(_, 0)}.toMap, shufflePairing.flatten.map{(_, true)}, shufflePairing, ban, shufflePairing, round, previousRound)
      i = i + 1
      // Keep the better solution
      if(Constraints.computeRoundScore(round, tryAlgo, previousRound) > Constraints.computeRoundScore(round, acc, previousRound)) tryAlgo
      else acc
    }
    Constraints.displayRoundReport(bestOfNRun, round, previousRound)
    bestOfNRun
  }
}