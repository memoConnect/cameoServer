package models

import java.util.Date
import traits.Model
import play.api.libs.json._
import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import helper.JsonHelper._
import helper.{ MongoCollections, IdHelper }

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 11:31 AM
 */
case class TwoFactorToken(id: MongoId,
                          identityId: MongoId,
                          created: Date) {
  def toJson: JsValue = Json.toJson(this)(TwoFactorToken.outputWrites)
}

object TwoFactorToken extends Model[TwoFactorToken] {

  val col = MongoCollections.twoFactorTokenCollection

  implicit val mongoFormat: Format[TwoFactorToken] = createMongoFormat(Json.reads[TwoFactorToken], Json.writes[TwoFactorToken])

  def docVersion = 0

  def evolutions = Map()

  def outputWrites = Writes[TwoFactorToken] {
    t =>
      Json.obj("token" -> t.id.toJson) ++
        addCreated(t.created)
  }

  def create(identityId: MongoId): TwoFactorToken = {
    TwoFactorToken(IdHelper.generateAccessToken(), identityId, new Date)
  }

  def createAndInsert(identityId: MongoId): TwoFactorToken = {
    val newToken = TwoFactorToken.create(identityId)
    TwoFactorToken.col.insert(newToken)
    newToken
  }

  override def createDefault(): TwoFactorToken = {
    TwoFactorToken.create(IdHelper.generateIdentityId())
  }
}

case class TwoFactorSmsKey(id: MongoId,
                           identityId: MongoId) {
  def toJson: JsValue = Json.toJson(this)(TwoFactorSmsKey.outputWrites)

  override def toString: String = this.id.id

  def delete: Future[Boolean] = {
    val query = Json.obj("_id" -> this.id)
    TwoFactorSmsKey.col.remove(query).map {
      _.ok
    }
  }
}

object TwoFactorSmsKey extends Model[TwoFactorSmsKey] {

  val col = MongoCollections.twoFactorSmsKeyCollection

  implicit val mongoFormat: Format[TwoFactorSmsKey] = createMongoFormat(Json.reads[TwoFactorSmsKey], Json.writes[TwoFactorSmsKey])

  def docVersion = 0

  def evolutions = Map()

  def outputWrites = Writes[TwoFactorSmsKey] {
    t =>
      Json.obj("key" -> t.id.toJson)
  }

  def create(identityId: MongoId): TwoFactorSmsKey = {
    val key = new TwoFactorSmsKey(IdHelper.generateTwoFactorSmsKey(), identityId)
    col.insert(key)
    key
  }

  override def createDefault(): TwoFactorSmsKey = {
    TwoFactorSmsKey.create(IdHelper.generateIdentityId())
  }
}

