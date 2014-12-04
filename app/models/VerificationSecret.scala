package models

import java.util.Date
import scala.concurrent.ExecutionContext.Implicits.global
import helper.IdHelper
import helper.MongoCollections._
import play.api.libs.json._
import traits.Model

import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 7:15 PM
 */

case class VerificationSecret(id: MongoId,
                              code: String,
                              accountId: MongoId,
                              valueType: String,
                              valueToBeVerified: String,
                              created: Date)

object VerificationSecret extends Model[VerificationSecret] {

  implicit val mongoFormat: Format[VerificationSecret] = createMongoFormat(Json.reads[VerificationSecret], Json.writes[VerificationSecret])

  val col = verificationCollection

  def docVersion = 0

  def evolutions = Map()

  def create(accountId: MongoId, valueToBeVerified: String, verificationType: String): VerificationSecret = {
    new VerificationSecret(
      IdHelper.generateVerificationSecret(),
      IdHelper.generateVerificationCode(),
      accountId,
      verificationType,
      valueToBeVerified,
      new Date)
  }

  def createAndInsert(accountId: MongoId, valueToBeVerified: String, verificationType: String): VerificationSecret = {
    val secret = create(accountId, valueToBeVerified, verificationType)

    // delete any secretes with same account and verification type, then insert the new one
    val query = Json.obj("accountId" -> accountId, "valueType" -> verificationType)
    VerificationSecret.deleteAll(query).map {
      le => VerificationSecret.insert(secret)
    }
    secret
  }

  override def createDefault(): VerificationSecret = {
    VerificationSecret.create(IdHelper.generateIdentityId(), "", "")
  }

  def findByCode(code: String): Future[Option[VerificationSecret]] = {
    val query = Json.obj("code" -> code)
    VerificationSecret.find(query)
  }
}