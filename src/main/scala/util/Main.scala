package util

import algorithm.{Manager, ReduceAlgorithm, ShuffleAlgorithm}
import com.typesafe.config.ConfigFactory
import domain.{Subscriber, Tools}
import ui.{MainFrame, TabTextLog}

/**
 * Created by vdoquang on 20/01/16.
 * 
 * Table Pairing Algorithm
 * 
 */
object Main extends Tools {
  def main(args: Array[String]) {
    FileOperation.loadConfigurationFile("sdajce.pairing")
    MainFrame.getInstance()
  }
}

object Conf {
  val configuration = ConfigFactory.load()

  val sizeOfTable = configuration.getInt("conf.tournament.sizeOfTable")
  val numberOfRound = configuration.getInt("conf.tournament.numberOfRound")
  val maxTry = configuration.getInt("conf.maxTry")
  val logLevel = configuration.getInt("conf.logLevel")

  val groupScoreWeight = configuration.getInt("conf.score.group")
  val alreadySeenScoreWeight = configuration.getInt("conf.score.alreadySeen")
  val constrainstSpecificScoreWeight = configuration.getInt("conf.score.constraintSpecific")
}


