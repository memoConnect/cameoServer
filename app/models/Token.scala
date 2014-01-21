package models

import java.util.Date
import traits.{Model, MongoHelper}
import play.api.libs.json._
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import reactivemongo.api.indexes.{IndexType, Index}
import play.modules.reactivemongo.json.collection.JSONCollection
import helper.IdHelper

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 9:31 PM
 */
case class Token(
                  id: MongoId,
                  identityId: MongoId,
                  created: Date
                  ) {
  def toJson: JsValue = Json.toJson(this)(Token.outputWrites)
}

object Token extends MongoHelper with Model[Token] {

  implicit lazy val col: JSONCollection = mongoDB.collection[JSONCollection]("tokens")

  implicit val mongoFormat: Format[Token] = createMongoFormat(Json.reads[Token], Json.writes[Token])

  def outputWrites = Writes[Token] {
    t =>
      Json.obj("token" -> t.id.toJson) ++
        addCreated(t.created)
  }

  def find(id: MongoId): Future[Option[Token]] = {
    val query = Json.obj("_id" -> id)
    col.find(query).one[Token]
  }

  def create(id: MongoId): Token = {
    new Token(
      new MongoId(IdHelper.generateAccessToken()),
      id,
      new Date)
  }
}
