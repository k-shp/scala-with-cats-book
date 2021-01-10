mport cats.data.Reader
import cats.syntax.applicative._

object ReaderMonadExercise {
  final case class Db(usernames: Map[Int, String], passwords: Map[String,
String])
  type DbReader[A] = Reader[Db, A]5
  
  def findUsername(userId: Int): DbReader[Option[String]] = {
    Reader(db => db.usernames.get(userId))
  }
  
  def checkPassword(username: String, password: String): DbReader[Boolean] = {
    Reader(db => db.passwords.get(username).contains(password))
  }3
  
  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
      for {
        username <- findUsername(userId)
        passwordMatch <- username.map(u => checkPassword(u,
password)).getOrElse(false.pure[DbReader])
      } yield passwordMatch
  }
  
  val users = Map(16 -> "dade", 2 -> "kate", 3 -> "margo")
  val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" ->
"secret")
  val db = Db(users, passwords)                                  
  
  
  def main(args: Array[String]): Unit = {
    checkLogin(1, "zerocool").run(db)
  }
}
