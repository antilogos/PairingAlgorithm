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

  override def toString = {
    s"$id"
  }
}

class ConstraintsLOTRLCG(name: String, id: String, threat: Int, wisdom: Int, attack: Int, defense: Int, hitpoint: Int, sphere: String)
  extends Constraints(id, Map("name"->name,"threat"->threat, "wisdom"-> wisdom, "attack"->attack, "defense"->defense, "hitpoint"->hitpoint, "sphere"->sphere)) {
}