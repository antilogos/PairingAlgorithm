package util

import java.io.{File, FileNotFoundException, IOException, PrintWriter}

import domain.{Constraints, Subscriber, Tools}

import scala.io.Source
/**
 * Created by vdoquang on 27/01/16.
 *
 * Operations to load a subscriber file and export the final pairing
 */
object FileOperation extends Tools {

  val separator = Conf.configuration.getString("conf.file.separator")

  def loadConfigurationFile(filename: String) = {
    logger(DEBUG, "is it loading the file ?")
    var separator = "\t"
    try {
      Source.fromInputStream(FileOperation.getClass.getResourceAsStream(s"/$filename"), "UTF-8").getLines().foreach {
        case line: String if line.startsWith("separator=") => separator = line.replaceAll("separator=(.)", "$1")
        case line: String if line.startsWith("|") => Constraints.identityRules = line.substring(1)
        case line: String if line.startsWith(":") => Constraints.calculationRules = line.substring(1) :: Constraints.calculationRules
        case line: String if line.startsWith("#") => // nothing
        case line: String if line.startsWith("fields.constraints=") => Constraints.fields = line.substring(19).split(separator).toList.map(_.trim)
        case line: String if line.startsWith("fields.subscribers=") => Subscriber.fields = line.substring(19).split(separator).toList.map(_.trim)
        case line: String => Constraints.inventory += (line.split(separator).map(_.trim).zip(Constraints.fields).map(_.swap).toMap.get("ID").get -> line.split(separator).map(_.trim).zip(Constraints.fields).map(_.swap).toMap)
      }
      logger(INFO, s"Using configuration from $filename")
      logger(DEBUG,s"separator: $separator")
      logger(DEBUG, s"identityRules: ${Constraints.identityRules}")
      logger(DEBUG, s"calculationRules: ${Constraints.calculationRules}")
      logger(DEBUG, s"constraints fields: ${Constraints.fields}")
      logger(DEBUG, s"subscribers fields: ${Subscriber.fields}")
      logger(DEBUG, s"inventory: ${Constraints.inventory}")
    } catch {
      case ex: FileNotFoundException => logger(INFO, s"Couldn't find the file $filename.")
        Nil
      case ex: IOException => logger(INFO, s"Had an IOException trying to read the file $filename.")
        Nil
    }
  }

  def loadSubscriberFile(filename: String): List[Subscriber] = {
    try {
      //val constraintMap = ConstraintsLOTRLCG.list.map{constraint => (constraint.id, constraint)}.toMap
      val lines = Source.fromFile(filename,"UTF-8").getLines().filterNot(_.startsWith("#")).map(_.trim)
      val subscibers = lines.map(_.split(separator).map(_.trim).zip(Subscriber.fields).map(_.swap).toMap).map{new Subscriber(_)}.toList
      logger(INFO, s"Using list of subscribers from $filename")
      logger(DEBUG, s"subscribers list: $subscibers")
      subscibers
    } catch {
      case ex: FileNotFoundException => logger(INFO, s"Couldn't find the file $filename.")
        Nil
      case ex: IOException => logger(INFO, s"Had an IOException trying to read the file $filename.")
        Nil
    }
  }

  def loadSubscriberFromText(text: String): List[Subscriber] = {
    //val constraintMap = ConstraintsLOTRLCG.list.map{constraint => (constraint.id, constraint)}.toMap
    val subscibers = text.split("\n").filterNot(_.isEmpty).map(_.split(separator).map(_.trim).zip(Subscriber.fields).map(_.swap).toMap).map{new Subscriber(_)}.toList
    logger(INFO, "Using list of subscribers from text field")
    logger(DEBUG, s"subscribers list: $subscibers")
    subscibers
  }

  def printToScreen(roundPairing: List[List[List[Subscriber]]], outputFormat: Option[String] = None): String = {
    Subscriber.exportTournament(roundPairing, outputFormat.getOrElse(Conf.configuration.getString("conf.file.format")))
  }

  def saveSubscriberFile(filename: String, roundPairing: List[List[List[Subscriber]]]) = {
    try {
      val writer = new PrintWriter(new File(s"$filename.out"))
      writer.write(Subscriber.exportTournament(roundPairing, Conf.configuration.getString("conf.file.format")))
      writer.close()
    } catch {
      case ex: IOException => logger(INFO, s"Had an IOException trying to write the file $filename.out.")
    }
  }
}
