package models

import java.util.Date

import helper.IdHelper
import helper.JsonHelper._
import play.api.libs.json._
import traits.SubModel

/**
 * User: Björn Reimer
 * Date: 6/26/13
 * Time: 9:31 PM
 */
case class Token(id: MongoId,
                 created: Date) {

  def toJson: JsValue = Json.toJson(this)(Token.outputWrites)
}

object Token extends SubModel[Token, Identity] {

  def parentModel = Identity
  def elementName = "tokens"

  implicit val mongoFormat: Format[Token] = createMongoFormat(Json.reads[Token], Json.writes[Token])

  def docVersion = 0

  def evolutions = Map()

  def outputWrites = Writes[Token] {
    t =>
      Json.obj("token" -> t.id.toJson) ++
        addCreated(t.created)
  }

  def createDefault(): Token = {
    new Token(
      IdHelper.generateAccessToken(),
      new Date)
  }
}
