package config

case class Config() {

}
object Config{

  val FILENAME: String = "./ai_proof.csv"
  val CSVSCHEMA = "AI Name;score;AI Name2;score2"
}
