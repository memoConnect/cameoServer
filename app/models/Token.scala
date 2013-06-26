package models

import java.util.Date
import traits.{ModelHelper, MongoHelper}
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

object Token extends MongoHelper with ModelHelper {

  implicit val mongoFormat = createMongoFormat(Json.reads[Token], Json.writes[Token])

  val outputWrites = Writes[Token] {
    t =>
      Json.obj("token" -> t.token) ++
        Json.obj("username" -> t.username) ++
        addCreated(t.created)
  }
}
