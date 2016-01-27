/**
 * Created by vdoquang on 20/01/16.
 * 
 * Table Pairing Algorithm
 * 
 */
object Main extends Tools {

  val sizeOfTable = 3
  val numberOfRound = 3
  val logLevel = INFO

  def main(args: Array[String]) {
    val fileName = if(args.nonEmpty) args(0) else "randomtest"
    logger(INFO,s"Start using $fileName")
    val input: List[Subscriber] = if(args.nonEmpty) FileOperation.loadFile(fileName) else randomSeed() //XXX To change
    logger(DEBUG,"Initialization - all pairing are constraints driven")
    val compatibility: Map[Subscriber, List[Subscriber]] = PairingAlgorithm.buildCompatibilityMap(input)
    logger(DEBUG,"Initialization - all seating are false")
    val seating: List[(Subscriber, Boolean)] = input.map((_, false))

    val disposition = PairingAlgorithm.findPairing(seating, compatibility, Nil, Nil)

    if(disposition._1.isEmpty) {
    } else {
      logger(DEBUG,s"Initial pairing is:\n${disposition._1.map(table => table.map(_.id).mkString("\t")).mkString("\n")}")
      val roundDisposition = (1 to numberOfRound).toList.map { r =>
        logger(INFO,s"Round $r - Beggining")
        GeneticAlgorithm.arrangeSeating(disposition._1, disposition._2, r)
      }
      FileOperation.saveFile(fileName, roundDisposition)
    }
  }
}
