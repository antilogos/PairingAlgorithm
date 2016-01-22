import scala.collection.mutable
import scala.util.Random

/**
 * Created by vdoquang on 20/01/16.
 */
object Main {

  def openInscription = 9
  def sizeOfTable = 3

  def alphabet(i: Int): String = ('A' to 'Z')(i - 1).toString

  class Subscriber(val id: String, val registry: Map[String, String], val additionnal: List[String] = Nil) {

    def this(id: String, h1: String = null, h2: String = null, h3: String = null) {
      this(id, Map("h1"->h1, "h2"->h2, "h3"->h3).filter(_._2 != null))
    }

    override def toString : String = {
      s"Subscriber:[id=${id}, registry=[${registry.values.mkString(";")}]]"
    }

    def listH: List[String] = registry.filterKeys(key => key.equals("h1") || key.equals("h2") || key.equals("h3")).values.toList
  }

  def bestSuit(seating: List[(Subscriber, Boolean)], compatibilityMap: Map[Subscriber, List[Subscriber]], pairing: List[List[Subscriber]]): Either[List[Subscriber],List[Subscriber]] = {
    if(pairing.filter(_.size != sizeOfTable).nonEmpty) {
      // Prioritize completion of table (there should be only one)
      val group = pairing.filter(_.size != sizeOfTable).head
      println(s"Try to fill table ${group.mkString(", ")}")
      val eligibleMatch = seating.map(_._1)
        // Remove subscriber with constraint, this also remove the member of the group
        .filter(_.listH.intersect(group.flatMap(_.listH)).isEmpty)
      val subscriberScore = compatibilityMap.filterKeys(eligibleMatch.contains(_))
        // Filter available compatibility
        .filterKeys(group.foldLeft(seating.map(_._1))((acc, curr) => compatibilityMap.get(curr).get.intersect(acc)).contains(_))
        // Map and sort subscriber by number of available pairing
        .mapValues(_.size).toList.sortBy(_._2)
      if (subscriberScore.isEmpty) {
        println(s"Group ${group.mkString(", ")} cannot be together, there is no match to fill")
        Left(group)
      } else {
        // Extract the first score and returns it with the group
        val bestOne = subscriberScore.head._1
        val table = bestOne :: group
        if(compatibilityMap.filterKeys(sub => !table.contains(sub)).mapValues(_.diff(table)).filter(_._2.size == 0).nonEmpty) {
          println(s"Group ${group.mkString(", ")} cannot be together, there would be somebody left")
          Left(group)
        }
        println(s"Adding ${bestOne} to the group")
        Right(bestOne :: group)
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
      println(s"${bestOne} as been selected, match remaining : ${compatibilityMap.get(bestOne).get.mkString(", ")}")
      if(compatibilityMap.get(bestOne).get.intersect(subscriberScore.map(_._1).tail).isEmpty) {
        println("Current pairing does not satisfy all subscriber")
        Left(List(bestOne))
      } else {
        val bestTwo = compatibilityMap.get(bestOne).get.intersect(subscriberScore.map(_._1).tail).head
        Right(List[Subscriber](bestOne, bestTwo))
      }
    }
  }

  def appair(seating: List[(Subscriber, Boolean)], compatibilityMap: Map[Subscriber, List[Subscriber]], pairing: List[List[Subscriber]], ban: List[List[Subscriber]]): List[List[Subscriber]] = {
    // Verify stop condition : all seated
    if(seating.filterNot(_._2).isEmpty) {
      // Success
      pairing
    } else {
      println(s"running pairing algorithm, set items : ${pairing.filter(_.size == sizeOfTable).map(table => table.map(sub => s"${sub.id}-${sub.listH.mkString(",")}").mkString("[",", ","]")).mkString(";")}")
      val hypothesis: Either[List[Subscriber],List[Subscriber]] = bestSuit(seating, compatibilityMap, pairing)
      hypothesis match {
        case Right(table) if (table.size == sizeOfTable) => println("Define new seating")
          // Seat all subscriber
          val newSeating: List[(Subscriber, Boolean)] = seating.map(seat => if (table.contains(seat._1)) (seat._1, true) else seat)
          // Remove all occurences of the candidates from the compatibility list
          val newCompatibilityMap = compatibilityMap.filterKeys(sub => !table.contains(sub)).mapValues(_.diff(table))
          // Remove temp table
          val newPairing = table :: pairing.filter(_.size == sizeOfTable)
          appair(newSeating, newCompatibilityMap, newPairing, ban)
        case Right(pair) => println("Pairing " + pair.map(_.id).mkString(" with "))
          appair(seating, compatibilityMap, pair :: pairing, ban)
        case Left(pair) if (pair.size > 1) => println("Hypothesis rejected")
          // Remove the potential matching
          val newCompatibilityMap = compatibilityMap.filterKeys(sub => pair.contains(sub)).mapValues(_.diff(pair))
          // Remove temp table
          val newPairing = pairing.filter(_.size == sizeOfTable)
          appair(seating, newCompatibilityMap, newPairing, ban)
        case Left(pair) if (pair.size == 1) =>
          if (pairing.isEmpty) {
            println("End of Algorithm, no solution was found")
            Nil
          } else {
            // Break the table
            val newSeating = seating.map(seat => (seat._1, false))
            // Add ban
            val newBan = pairing.head.take(2) :: ban
            println(s"No solution found with this pairing, adding ban on ${newBan.map{pair => pair.map(sub => sub.id)}.mkString(", ")} and retrying")
            // Construct the new compatibility map
            val newCompatibilityMap = buildCompatibilityMap(seating.map(_._1), newBan)
            println(s"New compatibility map is ${newCompatibilityMap}")
            appair(newSeating, newCompatibilityMap, Nil, newBan)
          }
      }
    }
  }


  def main(args: Array[String]) {
    println("Start")
    val input: List[Subscriber] = randomSeed()
    val compatibility: Map[Subscriber, List[Subscriber]] = buildCompatibilityMap(input, Nil)
    println("Initialization - all seating are false")
    val seating: List[(Subscriber, Boolean)] = input.map((_, false))

    val pairing = appair(seating, compatibility, Nil, Nil)

    println("pairing is : " + pairing.map(table => table.mkString("[",",","]")).mkString("\n"))

  }

  def buildCompatibilityMap(input: List[Subscriber], ban: List[List[Subscriber]]): Map[Subscriber, List[Subscriber]] = {
    // Put all subscriber available to all subscriber except themself
    Map(input.map{register => (register -> input.filterNot(_.equals(register)))}.toSeq: _*)
      // Filter incompatibility
      .map{case (sub, possibleMatch) => (sub, possibleMatch.filter(current => sub.listH.intersect(current.listH).isEmpty))}
      // Filter ban
      .map{case (sub, possibleMatch) => (sub, possibleMatch.filterNot(current => ban.filter(pair => pair.contains(sub)).flatten.contains(current)))}
  }

  def randomSeed(): List[Subscriber] = {
    println("Random Initialization is on")
    val randomH = (1 to openInscription).toList.map{i => val j = Random.nextInt(26); ((j+1)%26+1, (j+2)%26+1, (j+3)%26+1)}
    val subscriberList = (1 to openInscription).toList.map{i => new Subscriber("toto"+i.toString,alphabet(randomH(i-1)._1),alphabet(randomH(i-1)._2),alphabet(randomH(i-1)._3))}
    var i = 0
    while (i < openInscription) {
      println(s"Subscriber ${i+1} \t : ${subscriberList(i).id} - ${subscriberList(i).registry.filter(entry => entry._1.equals("h1") || entry._1.equals("h2") || entry._1.equals("h3")).map{_._2}.mkString("[",",","]")}")
      i+=1
    }
    subscriberList
  }
}
