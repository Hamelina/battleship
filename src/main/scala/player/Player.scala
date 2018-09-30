package player
import ship._



/**
  *
  * @param _fleet The fleet owned by the player.
  * @param _name The name of the player. If the player is supposed to be an AI, the name is None.
  * @param _level If the player is an AI, it corresponds to the level of it. Otherwise it is None.
  */
case class Player (_fleet: List[Ship], _name: Option[String], _level: Option[Int], _score: Int){
  def fleet: List[Any] = _fleet
  def name: Option[String] = _name
  def level: Option[Int] = _level
  def score: Int = _score

  //is hittable (x,y)
  //returns either the x,y is hittable or not


  /*def hit(x: Int, y: Int, player: Player): Player= {
    //return the new Player with the modified fleet
  }*/
}
