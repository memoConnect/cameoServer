package legacy.v1

import actors.{ ConfirmPhoneNumber, ConfirmMail }
import controllers.AccountController._
import events.{ AccountUpdate, ContactUpdate }
import helper.ResultHelper._
import models._
import play.api.Play
import play.api.libs.json.{ Reads, JsObject, Json, JsValue }
import play.api.mvc.{ Result, Action }
import play.api.mvc.BodyParsers.parse
import play.modules.statsd.api.Statsd
import services.LocalizationMessages
import traits.ExtendedController
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.Play.current

/**
 * User: Björn Reimer
 * Date: 08.12.14
 * Time: 14:25
 */
object AccountController extends ExtendedController {

  // not depreciated anymore since we temporarily reverted back to the previous registration
  implicit val depreciated = false

  case class AdditionalValues(reservationSecret: String, displayName: Option[String])

  object AdditionalValues {
    val reads: Reads[AdditionalValues] = Json.reads[AdditionalValues]
  }

  def createAccount = Action.async(parse.tolerantJson) {
    request =>

      val jsBody: JsValue = request.body
      val lang = LocalizationMessages.getBrowserLanguage(request)

      validateFuture[AdditionalValues](jsBody, AdditionalValues.reads) {
        additionalValues =>
          validateFuture[Account](jsBody, Account.createReads()) {
            account =>
              {
                val accountLowerCase = account.copy(loginName = account.loginName.toLowerCase)

                def createAccountWithIdentity(identity: Identity): Future[Result] = {
                  // add support user
                  identity.addSupport()

                  Account.col.insert(accountLowerCase).flatMap {
                    lastError =>
                      lastError.ok match {
                        case true =>
                          // create statsd event when user is not a test user
                          val testUserPrefix = Play.configuration.getString("testUser.prefix").getOrElse("foo")
                          if (!accountLowerCase.loginName.startsWith(testUserPrefix.toLowerCase)) {
                            Statsd.increment("custom.account.create")
                          }
                          // send verification mails and sms
                          if (accountLowerCase.email.isDefined) {
                            actors.verificationRouter ! ConfirmMail(accountLowerCase.id, lang)
                          }
                          if (accountLowerCase.phoneNumber.isDefined) {
                            actors.verificationRouter ! ConfirmPhoneNumber(accountLowerCase.id, lang)
                          }

                          accountLowerCase.toJsonWithIdentities(identity.id).map(resOk[JsObject])
                        case false =>
                          Future(resServerError("MongoError: " + lastError))
                      }
                  }
                }

                AccountReservation.checkReservationSecret(account.loginName, additionalValues.reservationSecret).flatMap {
                  case false => Future(resBadRequest("invalid reservation secret"))
                  case true =>

                    // check if there is a token
                    request.headers.get("Authorization") match {
                      case None =>
                        // no token, create new identity
                        Identity.createAndInsert(Some(account.id), account.loginName, None, None, true, additionalValues.displayName)
                          .flatMap(createAccountWithIdentity)

                      case Some(token) =>
                        // there is a token, check if it belongs to an external user
                        Identity.findByToken(new MongoId(token)).flatMap {
                          case None                                           => Future(resBadRequest("invalid token"))
                          case Some(identity) if identity.accountId.isDefined => Future(resBadRequest("token belongs to a registered user"))
                          case Some(identity) =>

                            // find the user that added the external contact and add him as contact (if contact still exists)
                            val query = Json.obj("contacts.identityId" -> identity.id)
                            Identity.find(query).flatMap {
                              case None => storeAccount(accountLowerCase, Some(identity.id))
                              case Some(otherIdentity) =>
                                for {
                                // add other identity as contact
                                  addContact <- identity.addContact(Contact.create(otherIdentity.id))
                                  updateIdentity <- {
                                    // update identity
                                    val map = Map(
                                      "cameoId" -> account.loginName,
                                      "accountId" -> account.id,
                                      "isDefaultIdentity" -> true
                                    )
                                    Identity.update(identity.id, IdentityModelUpdate.fromMap(map))
                                    identity.addSupport()
                                  }
                                  deleteDetails <- identity.deleteDetails(deleteDisplayName = true)
                                  if updateIdentity
                                  eventsSend <- {
                                    // get updated identity todo: this can be done without another db request
                                    Identity.find(identity.id).map {
                                      case None => // do nothing
                                      case Some(i) =>
                                        otherIdentity.contacts.find(_.identityId.equals(identity.id)) match {
                                          case None => // do nothing
                                          case Some(contact) => actors.eventRouter ! ContactUpdate(otherIdentity.id, contact, i)
                                        }
                                    }
                                  }
                                  result <- storeAccount(accountLowerCase, Some(identity.id))
                                } yield {
                                  result
                                }
                            }
                        }
                    }
                }
              }
          }
      }
  }
}
