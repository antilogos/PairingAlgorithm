package algorithm

import domain.{Constraints, Subscriber, Tools}
import util.Conf
/**
 * The Reduce Algorithm will recursively search for the first possible pairing satisfying all constraints.
 * It will choose the most difficult subscribers to pair and try to pair them.
 * When there is no more match possible while subscriber are not all seated, it will ban the current position and roll back.
 * If all positions from the start are banned, the Algorithm will end in faillure.
 */
object ReduceAlgorithm extends Tools  {

  def findPairing(seating: List[(Subscriber, Boolean)], compatibilityMap: Map[Subscriber, List[Subscriber]], pairing: List[List[Subscriber]], ban: List[List[List[Subscriber]]]): (List[List[Subscriber]], List[List[List[Subscriber]]]) = {
    logger(TRACE,s" --- Reduce Algorithm --- ")
    //logger(4, s"current ban list is: $ban")
    // Verify stop condition : all seated
    if(seating.forall(_._2)) {
      logger(TRACE,"End of Pairing Algorithm, solution was found")
      // Success
      (pairing, ban)
    } else {
      logger(DEBUG,s"running pairing algorithm, set items : ${pairing.filter(_.size == Conf.sizeOfTable).map(table => table.map(sub => s"${sub.id}-${sub.constraints.mkString(",")}").mkString("[",", ","]")).mkString(";")}")
      logger(DEBUG,s"yet to fit items : ${seating.filterNot(_._2)}")
      logger(DEBUG,s"possible pairing : ${compatibilityMap.mkString(";")}")
      val hypothesis: Either[List[Subscriber],List[Subscriber]] = bestSuit(seating, compatibilityMap, pairing)
      logger(DEBUG, s"working on hypothesis $hypothesis")
      hypothesis match {
        case Right(table) if table.size == Conf.sizeOfTable => // Completing a table
          // Reject if the situation was banned
          if(isRestrictedPairing(pairing,ban)) {
            logger(DEBUG,s"This situation was banned, hypothesis rejected: $pairing out of $ban")
            // Choose one of the pairing of the table
            val pair = table.diff(pairing.filter(_.size != Conf.sizeOfTable).head.tail)
            // Remove the potential matching
            val newCompatibilityMap = compatibilityMap.filterKeys(sub => pair.contains(sub)).mapValues(_.diff(pair))
            // Remove temp table
            val newPairing = pairing.filter(_.size == Conf.sizeOfTable)
            findPairing(seating, newCompatibilityMap, newPairing, ban)
          } else {
            logger(TRACE,s"Pairing table ${table.map(_.id).mkString(" with ")}")
            // Seat all subscriber
            val newSeating: List[(Subscriber, Boolean)] = seating.map(seat => if (table.contains(seat._1)) (seat._1, true) else seat)
            // Remove all occurences of the candidates from the compatibility list
            val newCompatibilityMap = compatibilityMap.filterKeys(sub => !table.contains(sub)).mapValues(_.diff(table))
            // Remove temp table
            val newPairing = table :: pairing.filter(_.size == Conf.sizeOfTable)
            findPairing(newSeating, newCompatibilityMap, newPairing, ban)
          }
        case Right(pair) => // New table or adding subscriber to a group
          logger(TRACE,s"Pairing ${pair.map(_.id).mkString(" with ")}")
          val newPairing = pair :: pairing.filter(_.intersect(pair).isEmpty)
          findPairing(seating, compatibilityMap, newPairing, ban)
        case Left(pair) if pair.size > 1 => logger(DEBUG,"Hypothesis rejected")
          // Remove the potential matching
          val newCompatibilityMap = compatibilityMap.filterKeys(sub => pair.contains(sub)).mapValues(_.diff(pair))
          if(newCompatibilityMap.isEmpty) {
            // Running out of compatibility
            logger(TRACE,s"No more compatibility possible, no solution remaining")
            return (Nil, ban)
          }
          // Remove the incomplete table
          val newPairing = pairing.filter(_.intersect(pair).isEmpty)
          findPairing(seating, newCompatibilityMap, newPairing, ban)
        case Left(pair) if pair.size == 1 =>
          if (pairing.isEmpty) {
            logger(TRACE,"End of Pairing Algorithm, no solution was found")
            (Nil, ban)
          } else {
            // Break the tables
            val newSeating = seating.map(seat => (seat._1, false))
            // Add ban graph
            val newBan = pairing :: ban
            logger(TRACE,s"No solution found with this pairing, adding ban on ${pairing.map{pair => pair.map(sub => sub.id)}.mkString(", ")} and retrying")
            // Construct the new compatibility map
            val newCompatibilityMap = Manager.buildCompatibilityMap()
            logger(DEBUG,s"New compatibility map is $newCompatibilityMap")
            findPairing(newSeating, newCompatibilityMap, Nil, newBan)
          }
      }
    }
  }

  /**
   * Find among the available pairing, the most suitable one.
   *
   * @param seating the current seating
   * @param compatibilityMap the remaining available pairing
   * @param pairing the pairing already done
   * @return Right: a good pairing, Left: no pairing were found, so we rise a problem
   */
  private def bestSuit(seating: List[(Subscriber, Boolean)], compatibilityMap: Map[Subscriber, List[Subscriber]], pairing: List[List[Subscriber]]): Either[List[Subscriber],List[Subscriber]] = {
    if(pairing.exists(_.size != Conf.sizeOfTable)) {
      // Prioritize completion of table (there should be only one)
      val tableToFill = pairing.filter(_.size != Conf.sizeOfTable).head
      logger(DEBUG,s"Try to fill table ${tableToFill.mkString(", ")}")
      val eligibleMatch = seating.filterNot(_._2).map(_._1)
        // Remove subscriber with constraint, this also remove the member of the group
        .filter(_.constraints.map(_.getKey(Constraints.identityRules)).intersect(tableToFill.flatMap(_.constraints.map(_.getKey(Constraints.identityRules)))).isEmpty)
      val subscriberScore = compatibilityMap.filterKeys(eligibleMatch.contains(_))
        // Filter available compatibility
        .filterKeys(tableToFill.foldLeft(seating.map(_._1))((acc, curr) => compatibilityMap.get(curr).get.intersect(acc)).contains(_))
        // Map and sort subscriber by number of available pairing
        .mapValues(_.size).toList.sortBy(_._2)
      if (subscriberScore.isEmpty) {
        logger(DEBUG,s"Group ${tableToFill.mkString(", ")} cannot be together, there is no match to fill")
        Left(tableToFill)
      } else {
        // Extract the first score and returns it with the group
        val bestOne = subscriberScore.head._1
        val table = bestOne :: tableToFill
        if(compatibilityMap.filterKeys(sub => !table.contains(sub)).mapValues(_.diff(table)).exists(_._2.isEmpty)) {
          logger(DEBUG,s"Group ${tableToFill.mkString(", ")} cannot be together, there would be somebody left")
          return Left(tableToFill)
        }
        logger(DEBUG,s"Adding $bestOne to the group")
        Right(bestOne :: tableToFill)
      }
    } else {
      // New Table
      val subscriberScore = compatibilityMap
        // Remove already seated, even if not in there normaly
        .filterKeys(sub => seating.filter(seat => !seat._2).map(seat => seat._1).contains(sub))
        // Map and sort subscriber by number of available pairing
        .mapValues(_.size).toList.sortBy(_._2)
      // Extract the first score and returns it with its first available pairing
      val bestOne = subscriberScore.head._1
      logger(DEBUG,s"$bestOne as been selected, match remaining: ${compatibilityMap.get(bestOne).get.mkString(", ")}")
      if(compatibilityMap.get(bestOne).get.intersect(subscriberScore.map(_._1).tail).isEmpty) {
        logger(DEBUG,"Current pairing does not satisfy all subscriber")
        Left(List(bestOne))
      } else {
        val bestTwo = compatibilityMap.get(bestOne).get.intersect(subscriberScore.map(_._1).tail).head
        Right(List[Subscriber](bestOne, bestTwo))
      }
    }
  }

  /**
   * Find if a currentTable situation was banned because it couldn't seat everyone
   * @param currentTable the current situation
   * @param situations the situations that were banned
   * @return True if the pairing had already been found unsolvable
   */
  private def isRestrictedPairing(currentTable: List[List[Subscriber]], situations: List[List[List[Subscriber]]]) = {
    // Search a situation that match
    situations.exists{
      // There is no impossible table that didn't match, so all match the current situation
      situation => situation.forall{
        // There is at least one table that correspond to the impossible table
        impossibleTable => currentTable.exists(
          // There is a table that match
          table => table.diff(impossibleTable).isEmpty
        )
      }
    }
  }
}
