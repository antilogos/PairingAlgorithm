import com.typesafe.config.ConfigFactory
/**
 * Created by vdoquang on 20/01/16.
 * 
 * Table Pairing Algorithm
 * 
 */
object Main extends Tools {
  def main(args: Array[String]) {
    val fileName = if(args.nonEmpty) args(0) else "randomtest"
    logger(INFO,s"Start using $fileName")
    val input: List[Subscriber] = if(args.nonEmpty) FileOperation.loadFile(fileName) else randomSeed()
    // Complete the tournament with blank subscribers, who doesn't contribute to score, are compatible with everyone, are already seated and cannot move
    val listSubscriber = (if(input.size%Conf.sizeOfTable != 0) (1 to Conf.sizeOfTable - (input.size % Conf.sizeOfTable) ).toList.map{_=>Subscriber.blank} else Nil) ++ input
    // Initialization - all pairing are constraints driven
    val compatibility: Map[Subscriber, List[Subscriber]] = ReduceAlgorithm.buildCompatibilityMap(listSubscriber)
    // Initialization - all seating are false except for blank subscriber
    val seating: List[(Subscriber, Boolean)] = listSubscriber.map{
      case sub:Subscriber if sub.id.nonEmpty => (sub, false)
      case sub:Subscriber => (sub, true)
    }
    // Define base pairing as all blank subscriber alone. If there is more blank subscriber than table, distribute them
    val blankPairing = if(listSubscriber.count(_.id.isEmpty) > (input.size / Conf.sizeOfTable)) {
      var i = 0
      listSubscriber.filter(_.id.isEmpty).map{sub => i+=1; (i%(input.size / Conf.sizeOfTable),sub)}.groupBy(_._1).values.toList.map(_.map(_._2))
    } else listSubscriber.filter(_.id.isEmpty).map{List(_)}
    // Find a solution first and check its existance
    val disposition = ReduceAlgorithm.findPairing(seating, compatibility, blankPairing, Nil)
    if(disposition._1.isEmpty) {
      // No solution found
    } else {
      logger(DEBUG,s"Initial pairing is:\n${disposition._1.map(table => table.map(_.id).mkString("\t")).mkString("\n")}")
      val roundDisposition = (1 to Conf.numberOfRound).toList.foldLeft(List[List[List[Subscriber]]]()) { (previousRound, round) =>
        logger(INFO,s"Round $round - Beggining")
        // Add the situation in previous round
        ShuffleAlgorithm.arrangeSeating(disposition._1, disposition._2, round, previousRound) :: previousRound
      }
      FileOperation.saveFile(fileName, roundDisposition)
    }
  }
}

object Conf {
  val configuration = ConfigFactory.load()

  val sizeOfTable = configuration.getInt("conf.tournament.sizeOfTable")
  val numberOfRound = configuration.getInt("conf.tournament.numberOfRound")
  val logLevel = configuration.getInt("conf.logLevel")

  val groupScoreWeight = configuration.getInt("conf.score.group")
  val alreadySeenScoreWeight = configuration.getInt("conf.score.alreadySeen")
  val constrainstSpecificScoreWeight = configuration.getInt("conf.score.constraintSpecific")
}


