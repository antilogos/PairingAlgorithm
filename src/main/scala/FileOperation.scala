import java.io.{PrintWriter, IOException, FileNotFoundException, File}
import java.util.Properties

import scala.io.Source
/**
 * Created by vdoquang on 27/01/16.
 *
 * Operations to load a subscriber file and export the final pairing
 */
object FileOperation extends Tools {

  val separator = Main.configuration.getString("conf.file.separator")

  def configure() = {
    val reader = Source.fromURL(getClass.getResource("pairing.properties")).bufferedReader()
    try {
      new Properties().load(reader)
    } catch {
      case ex: Exception => println("No properties file found, using default properties")

    }
  }

  def loadFile(filename: String): List[Subscriber] = {
    try {
      val constraintMap = ConstraintsLOTRLCG.list.map{constraint => (constraint.id, constraint)}.toMap
      Source.fromFile(filename,"UTF-8").getLines().map(_.trim).filterNot(_.startsWith("#")).map(_.split(separator).map(_.trim)).map{ fileFields =>
        val fields = if(fileFields.length == 5) fileFields else Array.concat(fileFields, Array("", "", ""))
        new Subscriber(fields(0).trim,(1 to 3).toList.map{i => try {
          constraintMap.get(fields(i)).get
        } catch {
          case ex: NoSuchElementException => throw new Exception(s"Couldn't find constraints ${fields(i)}")
        }
        }, fields(4))
      }.toList
    } catch {
      case ex: FileNotFoundException => println(s"Couldn't find the file $filename.")
        Nil
      case ex: IOException => println(s"Had an IOException trying to read the file $filename.")
        Nil
    }
  }

  def saveFile(filename: String, roundPairing: List[List[List[Subscriber]]]) = {
    try {
      val writer = new PrintWriter(new File(s"$filename.out"))
      writer.write(Subscriber.exportTournament(roundPairing, Main.configuration.getString("conf.file.format")))
      writer.close()
    } catch {
      case ex: IOException => println(s"Had an IOException trying to write the file $filename.out.")
    }
  }
}
