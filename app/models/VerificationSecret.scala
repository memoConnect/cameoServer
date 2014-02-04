package models


import play.api.libs.json._
import helper.IdHelper
import traits.MongoHelper


/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 7:15 PM
 */

case class VerificationSecret(
                               id: MongoId,
                               identityId: MongoId,
                               verificationType: String,
                               valueToBeVerified: String
                               )

object VerificationSecret extends MongoHelper {

  implicit val mongoFormat: Format[VerificationSecret] = createMongoFormat(Json.reads[VerificationSecret], Json.writes[VerificationSecret])

  implicit def col = verificationCollection

  def create(identityId: MongoId, valueToBeVerified: String, verificationType: String): VerificationSecret = {
    new VerificationSecret(
      IdHelper.generateVerificationSecret(),
      identityId,
      verificationType,
      valueToBeVerified
    )
  }

}
