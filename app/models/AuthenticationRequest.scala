package models

import java.util.Date

import helper.{ JsonHelper, IdHelper }
import play.api.libs.functional.syntax._
import play.api.libs.json._
import traits.SubModel

/**
 * User: Björn Reimer
 * Date: 18.07.14
 * Time: 11:36
 */
case class AuthenticationRequest(id: MongoId,
                                 fromKeyId: String,
                                 toKeyId: String,
                                 encryptedTransactionSecret: String,
                                 created: Date,
                                 docVersion: Int) {
  def toJson: JsObject =
    Json.obj(
      "fromKeyId" -> this.fromKeyId,
      "toKeyId" -> this.toKeyId,
      "encryptedTransactionSecret" -> this.encryptedTransactionSecret,
      "id" -> id.toJson) ++
      JsonHelper.addCreated(this.created)
}

object AuthenticationRequest extends SubModel[AuthenticationRequest, Identity] {

  def parentModel = Identity

  def elementName: String = "authenticationRequests"

  implicit def mongoFormat: Format[AuthenticationRequest] =
    createMongoFormat(Json.reads[AuthenticationRequest], Json.writes[AuthenticationRequest])

  def createReads: Reads[AuthenticationRequest] = (
    Reads.pure[MongoId](IdHelper.generateMongoId()) and
    (__ \ 'fromKeyId).read[String] and
    (__ \ 'toKeyId).read[String] and
    (__ \ 'encryptedTransactionSecret).read[String] and
    Reads.pure[Date](new Date) and
    Reads.pure[Int](docVersion)
  )(AuthenticationRequest.apply _)

  def createDefault(): AuthenticationRequest =
    new AuthenticationRequest(IdHelper.generateMongoId(), "", "", "", new Date, docVersion)

  def docVersion: Int = 0

  def evolutions: Map[Int, Reads[JsObject]] = Map()
}
