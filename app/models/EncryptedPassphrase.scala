package models

import play.api.libs.json._
import traits.SubModel
import helper.IdHelper
import play.api.libs.json.JsObject
import scala.concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import play.api.libs.functional.syntax._

/**
 * User: Björn Reimer
 * Date: 04.04.14
 * Time: 15:14
 */
case class EncryptedPassphrase(id: MongoId,
                               keyId: String,
                               value: String,
                               docVersion: Int) {

  def toJson: JsObject = Json.toJson(this)(EncryptedPassphrase.outputWrites).as[JsObject]

}

object EncryptedPassphrase extends SubModel[EncryptedPassphrase, Conversation] {

  def parentModel = Conversation
  def elementName = "encPassList"

  implicit val mongoFormat: Format[EncryptedPassphrase] = createMongoFormat(Json.reads[EncryptedPassphrase], Json.writes[EncryptedPassphrase])

  def outputWrites: Writes[EncryptedPassphrase] = Writes {
    dp =>
      Json.obj("keyId" -> dp.keyId) ++
        Json.obj("encryptedPassphrase" -> dp.value)
  }

  def createReads: Reads[EncryptedPassphrase] = (
    Reads.pure[MongoId](IdHelper.generateMongoId()) and
    (__ \ 'keyId).read[String] and
    (__ \ 'encryptedPassphrase).read[String] and
    Reads.pure[Int](docVersion)
  )(EncryptedPassphrase.apply _)

  def evolutions = Map()

  def docVersion = 0

  override def createDefault(): EncryptedPassphrase = {
    new EncryptedPassphrase(IdHelper.generateMongoId(), "", "", 0)
  }

}