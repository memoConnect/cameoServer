package models

import java.util.Date
import traits.{ Model }
import play.api.libs.json._
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import reactivemongo.api.indexes.{ IndexType, Index }
import play.modules.reactivemongo.json.collection.JSONCollection
import helper.IdHelper
import helper.JsonHelper._

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 9:31 PM
 */
case class Token(id: MongoId,
                 created: Date) {

  def toJson: JsValue = Json.toJson(this)(Token.outputWrites)
}

object Token extends Model[Token] {

  val col = Identity.col

  implicit val mongoFormat: Format[Token] = createMongoFormat(Json.reads[Token], Json.writes[Token])

  def docVersion = 0

  def evolutions = Map()

  def outputWrites = Writes[Token] {
    t =>
      Json.obj("token" -> t.id.toJson) ++
        addCreated(t.created)
  }

  def create(): Token = {
    new Token(
      IdHelper.generateAccessToken(),
      new Date)
  }

}
