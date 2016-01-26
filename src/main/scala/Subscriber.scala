/**
 * Object describing a subscriber
 * @param id the id of the subscriber
 * @param constraints the list of mandatory constraints. Subscriber cannot be paired if they have at least one constraints in comon.
 */
class Subscriber(val id: String, val constraints: List[Constraints], val group: String) {

  override def toString : String = {
    s"[Subscriber:$id, Constraints=[${constraints.mkString(";")}]]"
  }
}

