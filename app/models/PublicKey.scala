package models

import java.util.Date

import helper.IdHelper
import helper.JsonHelper._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import reactivemongo.core.commands.LastError
import traits.SubModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 2/27/14
 * Time: 11:19 AM
 */
case class PublicKey(id: MongoId,
                     name: Option[String],
                     key: String,
                     keySize: Int,
                     signatures: Seq[Signature],
                     created: Date,
                     deleted: Boolean,
                     docVersion: Int) {

  def toJson(additionalPublicKeySignatures: Map[String, Signature] = Map()): JsObject = {
    val allSignatures = additionalPublicKeySignatures.get(this.id.id) match {
      case None => signatures
      case Some(signature) => signatures :+ signature
    }

    Json.toJson(this.copy(signatures = allSignatures))(PublicKey.outputWrites).as[JsObject]
  }

}

object PublicKey extends SubModel[PublicKey, Identity] {

  def parentModel = Identity
  def elementName = "publicKeys"

  implicit val mongoFormat: Format[PublicKey] = createMongoFormat(Json.reads[PublicKey], Json.writes[PublicKey])

  def createReads: Reads[PublicKey] = (

    // todo: generate fingerprint for id directly
    Reads.pure[MongoId](IdHelper.generateMongoId()) and
    (__ \ 'name).readNullable[String] and
    (__ \ 'key).read[String] and
    (__ \ 'keySize).read[Int] and
    Reads.pure[Seq[Signature]](Seq()) and
    Reads.pure[Date](new Date) and
    Reads.pure[Boolean](false) and
    Reads.pure[Int](docVersion)
  )(PublicKey.apply _)

  def outputWrites: Writes[PublicKey] = Writes {
    pk =>
      Json.obj("id" -> pk.id.toJson) ++
        maybeEmptyJson("name", pk.name) ++
        Json.obj("key" -> pk.key) ++
        Json.obj("keySize" -> pk.keySize) ++
        Json.obj("signatures" -> pk.signatures) ++
        addCreated(pk.created)
  }

  override def delete(identityId: MongoId, id: MongoId): Future[LastError] = {
    val query = Json.obj("_id" -> identityId, "publicKeys._id" -> id)
    val set = Json.obj("$set" -> Json.obj("publicKeys.$.deleted" -> true))
    parentModel.col.update(query, set)
  }

  def evolutions =
    Map(
      0 -> PublicKeyEvolutions.addKeySize,
      1 -> PublicKeyEvolutions.addDate,
      2 -> PublicKeyEvolutions.addSignatures,
      3 -> PublicKeyEvolutions.addDeletedFlag
    )

  def docVersion = 4

  override def createDefault(): PublicKey = {
    new PublicKey(IdHelper.generateMongoId, None, "", 0, Seq(), new Date, false, docVersion)
  }
}

case class PublicKeyUpdate(name: Option[String])

object PublicKeyUpdate {
  implicit val format: Format[PublicKeyUpdate] = Json.format[PublicKeyUpdate]
}

object PublicKeyEvolutions {

  val addKeySize: Reads[JsObject] = Reads {
    js =>
      {
        val keySize: Reads[JsObject] = __.json.update((__ \ 'keySize).json.put(JsNumber(0)))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(1)))
        js.transform(keySize andThen addVersion)
      }
  }

  val addDate: Reads[JsObject] = Reads {
    js =>
      {
        val keySize: Reads[JsObject] = __.json.update((__ \ 'created).json.put(Json.obj("$date" -> new Date)))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(2)))
        js.transform(keySize andThen addVersion)
      }
  }

  val addSignatures: Reads[JsObject] = Reads {
    js =>
      {
        val signatures: Reads[JsObject] = __.json.update((__ \ 'signatures).json.put(JsArray()))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(3)))
        js.transform(signatures andThen addVersion)
      }
  }

  val addDeletedFlag: Reads[JsObject] = Reads {
    js =>
      {
        val addFlag = __.json.update((__ \ 'deleted).json.put(JsBoolean(false)))
        val addVersion = __.json.update((__ \ 'docVersion).json.put(JsNumber(3)))
        js.transform(addFlag andThen addVersion)
      }
  }
}

case class Signature(keyId: String,
                     content: String) {
  def toJson = Json.toJson(this).as[JsObject]
}

object Signature {
  implicit val format = Json.format[Signature]
}
