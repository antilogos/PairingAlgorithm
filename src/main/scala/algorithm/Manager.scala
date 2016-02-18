package algorithm

import domain.{Constraints, Subscriber}
import util.{Conf, FileOperation}
import util.Main._

/**
 * Created by vdoquang on 18/02/16.
 */
object Manager {

  def defaultRun(input: List[Subscriber]):  String = {
    // Complete the tournament with blank subscribers, who doesn't contribute to score, are compatible with everyone, are already seated and cannot move
    val listSubscriber = (if (input.size % Conf.sizeOfTable != 0) (1 to Conf.sizeOfTable - (input.size % Conf.sizeOfTable)).toList.map { _ => Subscriber.blank } else Nil) ++ input
    // Initialization - all pairing are constraints driven
    val compatibility: Map[Subscriber, List[Subscriber]] = Manager.buildCompatibilityMap(listSubscriber)
    // Initialization - all seating are false except for blank subscriber
    val seating = Manager.buildInitialSeating(listSubscriber)
    // Define base pairing as all blank subscriber alone. If there is more blank subscriber than table, distribute them
    val blankPairing = Manager.buildInitialPairing(listSubscriber, input)
    // Find a solution first and check its existance
    val disposition = ReduceAlgorithm.findPairing(seating, compatibility, blankPairing, Nil)
    if (disposition._1.isEmpty) {
      // No solution found
      "No solution was found"
    } else {
      logger(DEBUG, s"Initial pairing is:\n${disposition._1.map(table => table.map(_.id).mkString("\t")).mkString("\n")}")
      val roundDisposition = (1 to Conf.numberOfRound).toList.foldLeft(List[List[List[Subscriber]]]()) { (previousRound, round) =>
        logger(INFO, s"\n ========== Round $round - Beggining ========== \n")
        // Add the situation in previous round
        ShuffleAlgorithm.arrangeSeating(disposition._1, disposition._2, round, previousRound) :: previousRound
      }
      FileOperation.printToScreen(roundDisposition)
    }
  }

  /**
   * Basic compatibility map. Use only mandatory constraints.
   * @param listSubscriber the list of subscribers
   * @return the list of all subscribers mapped with a list of all available pairing
   */
  def buildCompatibilityMap(listSubscriber: List[Subscriber]): Map[Subscriber, List[Subscriber]] = {
    // Put all subscriber available to all subscriber except themself
    Map(listSubscriber.map{register => register -> listSubscriber.filterNot(_.equals(register))}.toSeq: _*)
    // Filter incompatibility
    .map{case (sub, possibleMatch) => (sub, possibleMatch.filter(current => sub.constraints.map(_.getKey(Constraints.identityRules)).intersect(current.constraints.map(_.getKey(Constraints.identityRules))).isEmpty))}
  }

  /**
   *
   * @param listSubscriber
   * @return
   */
  def buildInitialSeating(listSubscriber: List[Subscriber]): List[(Subscriber, Boolean)] = {
    listSubscriber.map {
      case sub: Subscriber if sub.id.nonEmpty => (sub, false)
      case sub: Subscriber => (sub, true)
    }
  }

  def buildInitialPairing(listSubscriber: List[Subscriber], input: List[Subscriber]) : List[List[Subscriber]] = {
    if (listSubscriber.count(_.id.isEmpty) > input.size / Conf.sizeOfTable) {
      var i = 0
      listSubscriber.filter(_.id.isEmpty).map { sub => i += 1; (i % (input.size / Conf.sizeOfTable), sub) }.groupBy(_._1).values.toList.map(_.map(_._2))
    } else listSubscriber.filter(_.id.isEmpty).map {
      List(_)
    }
  }

}
