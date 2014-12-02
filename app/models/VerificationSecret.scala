package models

import java.util.Date

import helper.IdHelper
import helper.MongoCollections._
import play.api.libs.json._
import traits.Model

/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 7:15 PM
 */

case class VerificationSecret(id: MongoId,
                              accountId: MongoId,
                              valueType: String,
                              valueToBeVerified: String,
                              created: Date) {

  val verificationCodeLength = 6

  def getVerificationCode: String = this.id.id.substring(0,verificationCodeLength)

  def checkVerificationCode(code: String) = getVerificationCode.equals(code)

}

object VerificationSecret extends Model[VerificationSecret] {

  implicit val mongoFormat: Format[VerificationSecret] = createMongoFormat(Json.reads[VerificationSecret], Json.writes[VerificationSecret])

  val col = verificationCollection

  def docVersion = 0

  def evolutions = Map()

  def create(accountId: MongoId, valueToBeVerified: String, verificationType: String): VerificationSecret = {
    new VerificationSecret(
      IdHelper.generateVerificationSecret(),
      accountId,
      verificationType,
      valueToBeVerified,
      new Date)
  }

  override def createDefault(): VerificationSecret = {
    VerificationSecret.create(IdHelper.generateIdentityId(), "", "")
  }
}