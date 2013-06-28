package models

import java.util.Date
import traits.{Model, MongoHelper}
import play.api.libs.json._

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 9:31 PM
 */
case class Token(
                  token: String,
                  username: String,
                  isAdmin: Boolean,
                  created: Date
                  )

object Token extends MongoHelper with Model[Token] {

  implicit val collection = tokenCollection
  implicit val mongoFormat: Format[Token] = createMongoFormat(Json.reads[Token], Json.writes[Token])

  val inputReads = Json.reads[Token]

  val outputWrites = Writes[Token] {
    t =>
      Json.obj("token" -> t.token) ++
        Json.obj("username" -> t.username) ++
        addCreated(t.created)
  }
}
