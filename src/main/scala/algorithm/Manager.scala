package algorithm

import domain.{Constraints, Subscriber}
import util.{Conf, FileOperation}
import util.Main._

/**
 * Created by vdoquang on 18/02/16.
 */
object Manager {

  var subscriberList = List[Subscriber]()

  def defaultRun(input: List[Subscriber]):  String = {
    // Complete the tournament with blank subscribers, who doesn't contribute to score, are compatible with everyone, are already seated and cannot move
    subscriberList = (if (input.size % Conf.sizeOfTable != 0) (1 to Conf.sizeOfTable - (input.size % Conf.sizeOfTable)).toList.map { _ => Subscriber.blank } else Nil) ++ input
    // Initialization - all pairing are constraints driven
    val compatibility: Map[Subscriber, List[Subscriber]] = Manager.buildCompatibilityMap()
    // Initialization - all seating are false except for blank subscriber
    val seating = Manager.buildInitialSeating()
    // Define base pairing as all blank subscriber alone. If there is more blank subscriber than table, distribute them
    val blankPairing = Manager.buildInitialPairing(input)
    // Find a solution first and check its existance
    val disposition = ReduceAlgorithm.findPairing(seating, compatibility, blankPairing, Nil)
    if (disposition._1.isEmpty) {
      // No solution found
      "No solution was found"
    } else {
      logger(DEBUG, s"Initial pairing is:\n${disposition._1.map(table => table.map(_.id).mkString("\t")).mkString("\n")}")
      val roundDisposition = (1 to Conf.numberOfRound).toList.foldLeft(List[List[List[Subscriber]]]()) { (previousRoundPairing, round) =>
        logger(INFO, s"\n ========== Round $round - Beggining ========== \n")
        // Add the situation in previous round
        ShuffleAlgorithm.arrangeSeating(disposition._1, disposition._2, round, previousRoundPairing) :: previousRoundPairing
      }.reverse
      FileOperation.printToScreen(roundDisposition)
    }
  }

  /**
   * Basic compatibility map. Use only mandatory constraints.
   * @return the list of all subscribers mapped with a list of all available pairing
   */
  def buildCompatibilityMap(): Map[Subscriber, List[Subscriber]] = {
    // Put all subscriber available to all subscriber except themself
    Map(subscriberList.map{register => register -> subscriberList.filterNot(_.equals(register))}.toSeq: _*)
    // Filter incompatibility
    .map{case (sub, possibleMatch) => (sub, possibleMatch.filter(current => sub.constraints.map(_.getKey(Constraints.identityRules)).intersect(current.constraints.map(_.getKey(Constraints.identityRules))).isEmpty))}
  }

  /**
   *
   * @return
   */
  def buildInitialSeating(): List[(Subscriber, Boolean)] = {
    subscriberList.map {
      case sub: Subscriber if sub.id.nonEmpty => (sub, false)
      case sub: Subscriber => (sub, true)
    }
  }

  def buildInitialPairing(input: List[Subscriber]) : List[List[Subscriber]] = {
    if (subscriberList.count(_.id.isEmpty) > input.size / Conf.sizeOfTable) {
      var i = 0
      subscriberList.filter(_.id.isEmpty).map { sub => i += 1; (i % (input.size / Conf.sizeOfTable), sub) }.groupBy(_._1).values.toList.map(_.map(_._2))
    } else subscriberList.filter(_.id.isEmpty).map {
      List(_)
    }
  }

}
