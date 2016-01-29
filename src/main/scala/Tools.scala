import java.io.{IOException, File, PrintWriter}

import scala.util.Random

/**
 * Created by vdoquang on 20/01/16.
 * 
 * Table Pairing Algorithm
 * 
 */
trait Tools {

  def randomSeed(): List[Subscriber] = {
    def openInscription = Conf.sizeOfTable * 3
    def pickRandomConstraints(): Constraints = new Constraints(scala.util.Random.shuffle(Constraints.inventory.keys.toList).head)
    logger(INFO, "Random Initialization is on")
    val subscriberList = (1 to openInscription).toList.map { i => val groupName = fakeGroup(Random.nextInt(fakeGroup.size))
      new Subscriber((if (groupName.equals("")) "OPEN" else groupName) + i.toString, (1 to 3).toList.map{i => pickRandomConstraints()}, groupName) }
    var i = 0
    while (i < openInscription) {
      logger(TRACE, s"Subscriber ${i + 1} \t : ${subscriberList(i).id} - ${subscriberList(i).constraints.mkString("[", ",", "]")}")
      i += 1
    }
    try {
      val writer = new PrintWriter(new File("randomtest.in"))
      writer.write(subscriberList.map(_.toExport(Conf.configuration.getString("conf.file.format"))).mkString("[table border=\"1\"][tr]","[/tr]\n[tr]","[/tr][/table]"))
      writer.close()
    } catch {
      case ex: IOException => println(s"Had an IOException trying to write the file randomtest.in.")
    }
    subscriberList
  }

  val fakeGroup = List("", "ALPHA", "", "BETA", "", "CHARLIE", "", "DELTA", "", "ECHO", "", "FOXTROT", "", "GOLF", "", "HECTOR", "")

  val DEBUG: Integer = 0
  val TRACE: Integer = 1
  val INFO: Integer = 2

  def logger(level: Int, string: String) = {
    if (level >= Conf.logLevel) println(string)
  }
}
