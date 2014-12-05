package models

import java.util.Date

import helper.IdHelper
import helper.MongoCollections._
import play.api.libs.json._
import traits.Model

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 7:15 PM
 */

case class PasswordReset(id: MongoId,
                         code: String,
                         accountId: MongoId,
                         resetMethod: String,
                         created: Date)

object PasswordReset extends Model[PasswordReset] {

  implicit val mongoFormat: Format[PasswordReset] = createMongoFormat(Json.reads[PasswordReset], Json.writes[PasswordReset])

  val col = verificationCollection

  def docVersion = 0

  def evolutions = Map()

  def create(accountId: MongoId, resetMethod: String): PasswordReset = {
    new PasswordReset(
      IdHelper.generatePasswordResetSecret(),
      IdHelper.generatePasswordResetCode(),
      accountId,
      resetMethod,
      new Date)
  }

  def createAndInsert(accountId: MongoId, resetMethod: String): PasswordReset = {
    val reset = create(accountId, resetMethod)

    // delete any secretes with same account and reset method, then insert the new one
    val query = Json.obj("accountId" -> accountId, "resetMethod" -> resetMethod)
    PasswordReset.deleteAll(query).map {
      le => PasswordReset.insert(reset)
    }
    reset
  }

  override def createDefault(): PasswordReset = {
    PasswordReset.create(IdHelper.generateIdentityId(), "")
  }

  def findByCode(code: String): Future[Option[PasswordReset]] = {
    val query = Json.obj("code" -> code)
    PasswordReset.find(query)
  }
}