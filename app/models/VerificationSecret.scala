package models

import play.api.libs.json._
import helper.{ JsonHelper, IdHelper }
import java.util.Date
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import traits.Model
import helper.MongoCollections._

/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 7:15 PM
 */

case class VerificationSecret(id: MongoId,
                              identityId: MongoId,
                              verificationType: String,
                              valueToBeVerified: String,
                              created: Date)

object VerificationSecret extends Model[VerificationSecret] {

  implicit val mongoFormat: Format[VerificationSecret] = createMongoFormat(Json.reads[VerificationSecret], Json.writes[VerificationSecret])

  implicit def col = verificationCollection

  def docVersion = 0

  def evolutions = Map()

  def create(identityId: MongoId, valueToBeVerified: String, verificationType: String): VerificationSecret = {
    new VerificationSecret(
      IdHelper.generateVerificationSecret(),
      identityId,
      verificationType,
      valueToBeVerified,
      new Date)
  }

  override def createDefault(): VerificationSecret = {
    VerificationSecret.create(IdHelper.generateIdentityId(), "", "")
  }
}
