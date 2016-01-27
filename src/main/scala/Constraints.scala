/**
 * Constraints object.
 *
 * Subscriber cannot be paired with the same constraints id.
 * Constraints have others attributes that should be evenly distributed around tables
 *
 */
class Constraints(val id: String, val attributes: Map[String, Any]) {
  def getNum(key: String): Int = {
    attributes.get(key).get match {case i: Int => i}
  }

  def getKey(key: String): String = attributes.get(key).get.toString

  def toExport(format: String) = {
    s"$id"
  }
  override def toString = {
    s"$id"
  }
}

object Constraints {

  def calculateScorePenalty(constraintList: List[List[Constraints]]): Int = {
    Main.configuration.getInt("conf.score.constraintSpecific")
  }
}

class ConstraintsLOTRLCG(name: String, id: String, threat: Int, wisdom: Int, attack: Int, defense: Int, hitpoint: Int, sphere: String)
  extends Constraints(id, Map("name"->name,"threat"->threat, "wisdom"-> wisdom, "attack"->attack, "defense"->defense, "hitpoint"->hitpoint, "sphere"->sphere)) {

  override def toExport(format: String) = {
    format match {
      case "txt" => s"$id"
      case "bbc" => s"$id :${sphere match {case "T" => "tactique" case "R" => "connaissance" case "L" => "commandement" case "S" => "energie" case "N" => "gandalf"}}:"
    }
  }
}

object ConstraintsLOTRLCG {
  def calculateScorePenalty(constraintList: List[List[Constraints]]): Int = {
    // Evenly repartition of sphere is important - score from 0 to 60
    val sphereDiff = constraintList.flatten.groupBy(_.getKey("sphere")).mapValues(_.size)
    val sphereScore = List((sphereDiff.values.max - sphereDiff.values.min) * 5, 60).min
    // Table lacking a Sphere - bonus penalty of 20
    val lackScore = if(sphereDiff.values.min == 0) 20 else 0
    // Evenly repartition of threat is important - score from 0 to 20
    val threatMean = constraintList.map(_.map(_.getNum("threat")).sum).sum / Main.sizeOfTable
    val threatScore = List(List(threatMean - 27, 27 - threatMean).max, 40).min
    // Score over 100
    sphereScore + lackScore + threatScore
  }

  val list = List(new ConstraintsLOTRLCG("Amarthiul","Amarthiúl",10,1,3,3,3,"L"),
    new ConstraintsLOTRLCG("Aragorn","Aragorn I",12,2,3,2,5,"L"),
    new ConstraintsLOTRLCG("Balin","Balin",9,2,1,2,4,"L"),
    new ConstraintsLOTRLCG("Boromir","Boromir II",11,1,3,2,5,"L"),
    new ConstraintsLOTRLCG("Celeborn","Celeborn",11,3,2,2,4,"L"),
    new ConstraintsLOTRLCG("Dain","Dáin Ironfoot",11,1,2,3,5,"L"),
    new ConstraintsLOTRLCG("Elrohir","Elrohir",10,2,2,1,4,"L"),
    new ConstraintsLOTRLCG("Erkenbrand","Erkenbrand",10,1,2,3,4,"L"),
    new ConstraintsLOTRLCG("Faramir","Faramir II",11,2,2,2,5,"L"),
    new ConstraintsLOTRLCG("Gloin","Glóin",9,2,2,1,4,"L"),
    new ConstraintsLOTRLCG("Halbarad","Halbarad",10,2,2,2,4,"L"),
    new ConstraintsLOTRLCG("Hirluin","Hirluin the Fair",8,1,1,1,4,"L"),
    new ConstraintsLOTRLCG("Imrahil","Prince Imrahil",11,2,3,2,4,"L"),
    new ConstraintsLOTRLCG("Sam","Sam Gamgee",8,3,1,1,3,"L"),
    new ConstraintsLOTRLCG("Theodred","Théodred",8,1,2,1,4,"L"),
    new ConstraintsLOTRLCG("Thorin","Thorin Oakenshield",12,3,3,1,5,"L"),
    new ConstraintsLOTRLCG("Aragorn","Aragorn III",12,2,3,2,5,"T"),
    new ConstraintsLOTRLCG("Bard","Bard the Bowman",11,2,3,2,4,"T"),
    new ConstraintsLOTRLCG("Beorn","Beorn",12,0,5,1,10,"T"),
    new ConstraintsLOTRLCG("Beregond","Beregond",10,0,1,4,4,"T"),
    new ConstraintsLOTRLCG("Boromir","Boromir I",11,1,3,2,5,"T"),
    new ConstraintsLOTRLCG("Brand","Brand son of Bain",10,2,3,2,3,"T"),
    new ConstraintsLOTRLCG("Dori","Dori",10,1,2,2,5,"T"),
    new ConstraintsLOTRLCG("Elladan","Elladan",10,2,1,2,4,"T"),
    new ConstraintsLOTRLCG("Eomer","Éomer",10,1,3,2,4,"T"),
    new ConstraintsLOTRLCG("Gimli","Gimli",11,2,2,2,5,"T"),
    new ConstraintsLOTRLCG("Hama","Háma",9,1,3,1,4,"T"),
    new ConstraintsLOTRLCG("Legolas","Legolas",9,1,3,1,4,"T"),
    new ConstraintsLOTRLCG("Mablung","Mablung",10,2,2,2,4,"T"),
    new ConstraintsLOTRLCG("Merry","Merry I",6,2,0,1,2,"T"),
    new ConstraintsLOTRLCG("Thalin","Thalin",9,1,2,2,4,"T"),
    new ConstraintsLOTRLCG("Theoden","Théoden I",12,2,3,2,4,"T"),
    new ConstraintsLOTRLCG("Arwen","Arwen Undómiel",9,3,1,2,3,"S"),
    new ConstraintsLOTRLCG("Caldara","Caldara",8,2,1,2,3,"S"),
    new ConstraintsLOTRLCG("Dunhere","Dúnhere",8,1,2,1,4,"S"),
    new ConstraintsLOTRLCG("Dwalin","Dwalin",9,1,2,2,4,"S"),
    new ConstraintsLOTRLCG("Eleanor","Eleanor",7,1,1,2,3,"S"),
    new ConstraintsLOTRLCG("Eowyn","Éowyn",9,4,1,1,3,"S"),
    new ConstraintsLOTRLCG("Bolger","Fatty Bolger",7,1,1,2,3,"S"),
    new ConstraintsLOTRLCG("Frodon","Frodo Baggins",7,2,1,2,2,"S"),
    new ConstraintsLOTRLCG("Galadriel","Galadriel",9,4,0,0,4,"S"),
    new ConstraintsLOTRLCG("Glorfindel","Glorfindel II",5,3,3,1,5,"S"),
    new ConstraintsLOTRLCG("Idraen","Idraen",11,2,3,2,4,"S"),
    new ConstraintsLOTRLCG("Merry","Merry II",6,2,1,1,2,"S"),
    new ConstraintsLOTRLCG("Nori","Nori",9,2,1,2,4,"S"),
    new ConstraintsLOTRLCG("Oin","Oin",8,2,1,1,4,"S"),
    new ConstraintsLOTRLCG("Pippin","Pippin I",6,2,1,1,2,"S"),
    new ConstraintsLOTRLCG("Theoden","Théoden II",12,2,3,2,4,"S"),
    new ConstraintsLOTRLCG("Aragorn","Aragorn II",12,2,3,2,5,"R"),
    new ConstraintsLOTRLCG("Beravor","Beravor",10,2,2,2,4,"R"),
    new ConstraintsLOTRLCG("Bifur","Bifur",7,2,1,2,3,"R"),
    new ConstraintsLOTRLCG("Bilbon","Bilbo Baggins",9,1,1,2,2,"R"),
    new ConstraintsLOTRLCG("Bombur","Bombur",8,0,1,2,5,"R"),
    new ConstraintsLOTRLCG("Damrod","Damrod",9,2,2,1,4,"R"),
    new ConstraintsLOTRLCG("Denethor","Denethor",8,1,1,3,3,"R"),
    new ConstraintsLOTRLCG("Elrond","Elrond",13,3,2,3,4,"R"),
    new ConstraintsLOTRLCG("Erestor","Erestor",10,2,2,2,4,"R"),
    new ConstraintsLOTRLCG("Faramir","Faramir I",11,2,2,2,5,"R"),
    new ConstraintsLOTRLCG("Glorfindel","Glorfindel I",12,3,3,1,5,"R"),
    new ConstraintsLOTRLCG("Gríma","Gríma",9,2,1,2,3,"R"),
    new ConstraintsLOTRLCG("Haldir","Haldir of Lórien",9,2,3,1,3,"R"),
    new ConstraintsLOTRLCG("Mirlonde","Mirlonde",8,2,2,1,3,"R"),
    new ConstraintsLOTRLCG("Ori","Ori",8,2,2,1,3,"R"),
    new ConstraintsLOTRLCG("Pippin","Pippin II",6,2,1,1,2,"R"),
    new ConstraintsLOTRLCG("Rossiel","Rossiel",8,2,1,2,3,"R"),
    new ConstraintsLOTRLCG("Sylvebarbe","Treebeard",13,2,3,3,5,"R"),
    new ConstraintsLOTRLCG("Gandalf","Gandalf",14,3,3,3,5,"N"))
}