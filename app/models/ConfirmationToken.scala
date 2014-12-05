package models

import java.util.Date

import helper.IdHelper
import helper.MongoCollections._
import play.api.Logger
import play.api.libs.json._
import traits.Model

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 7:15 PM
 */

case class ConfirmationToken(id: MongoId,
                             code: String,
                             accountId: MongoId,
                             confirmationType: String,
                             confirmationPath: String,
                             confirmationAddress: String,
                             created: Date)

object ConfirmationToken extends Model[ConfirmationToken] {

  implicit val mongoFormat: Format[ConfirmationToken] = createMongoFormat(Json.reads[ConfirmationToken], Json.writes[ConfirmationToken])

  val col = confirmationCollection

  def docVersion = 0

  def evolutions = Map()

  def create(accountId: MongoId, confirmationType: String, confirmationPath: String, confirmationValue: String): ConfirmationToken = {
    new ConfirmationToken(
      IdHelper.generateConfirmationSecret(),
      IdHelper.generateConfirmationCode(),
      accountId,
      confirmationType,
      confirmationPath,
      confirmationValue,
      new Date)
  }

  def createAndInsert(accountId: MongoId, confirmationType: String, confirmationPath: String, confirmationValue: String): ConfirmationToken = {
    val secret = create(accountId, confirmationType, confirmationPath, confirmationValue)
    // delete any secretes with same account and verification type, then insert the new one
    val query = Json.obj("accountId" -> accountId, "confirmationType" -> confirmationType, "confirmationPath" -> confirmationPath)
    ConfirmationToken.deleteAll(query).map {
      le => ConfirmationToken.insert(secret)
    }
    secret
  }

  override def createDefault(): ConfirmationToken = {
    ConfirmationToken.create(IdHelper.generateIdentityId(), "", "", "")
  }

  def findByCode(code: String): Future[Option[ConfirmationToken]] = {
    val query = Json.obj("code" -> code)
    ConfirmationToken.find(query)
  }
}

trait ConfirmResult
case object ConfirmSuccess extends ConfirmResult
case object ConfirmExpired extends ConfirmResult
case object ConfirmValueChanged extends ConfirmResult
case object ConfirmError extends ConfirmResult