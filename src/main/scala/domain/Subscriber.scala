package domain

/**
 * Object describing a subscriber
 * @param id the id of the subscriber
 * @param constraints the list of mandatory constraints. domain.Subscriber cannot be paired if they have at least one constraints in comon.
 */
class Subscriber(val id: String, val constraints: List[Constraints], val group: String) {

  def this(attribute: Map[String, String]) {
    this(attribute.getOrElse("ID",""),
      attribute.filterKeys(_.startsWith("C.")).values.map(new Constraints(_)).toList,
      attribute.getOrElse("GROUP",""))
  }

  override def toString : String = {
    s"[$id, {${constraints.mkString(";")}}]"
  }

  def toExport(format: String) = {
    format match {
      case "txt" => (id :: constraints.map(_.id) ++ List(group)).mkString("\t")
      case "bbc" => (id :: constraints.map(_.toExport(format)) ++ List(group)).mkString("[td]","[/td]\n[td]","[/td]\n")
    }
  }
}

object Subscriber {

  var fields: List[String] = List("ID", "GROUP")

  def blank = {
    new Subscriber("", Nil, "")
  }

  def exportTable(table: List[Subscriber], format: String) = {
    format match {
      case "txt" => table.map(_.toExport(format)).mkString("\n")
      case "bbc" => table.map(_.toExport(format)).mkString("[table border=\"1\"][tr]","[/tr][tr]\n","[/tr][/table]\n")
    }
  }

  def exportRound(round: List[List[Subscriber]], format: String) = {
    var numTable = 0
    round.map{ table =>
      numTable += 1
      s"Table $numTable:\n${exportTable(table, format)}"
    }.mkString("\n")
  }

  def exportTournament(tournament: List[List[List[Subscriber]]], format: String) = {
    var numRound = 0
    tournament.map{ round =>
      numRound += 1
      s"Round $numRound:\n${exportRound(round, format)}"
    }.mkString("\n")
  }
}
