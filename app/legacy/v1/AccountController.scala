package legacy.v1

import actors.{ConfirmPhoneNumber, ConfirmMail}
import controllers.AccountController._
import events.ContactUpdate
import helper.ResultHelper._
import models._
import play.api.Play
import play.api.libs.json.{JsObject, Json, JsValue}
import play.api.mvc.{Result, Action}
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

  implicit val depreciated = true

  def createAccount = Action.async(parse.tolerantJson) {
    request =>

      val jsBody: JsValue = request.body
      val lang = LocalizationMessages.getBrowserLanguage(request)

      validateFuture[AdditionalValues](jsBody, AdditionalValues.reads) {
        additionalValues =>
          validateFuture[Account](jsBody, Account.createReads()) {
            account =>
            {
              def createAccountWithIdentity(identity: Identity): Future[Result] = {
                val accountLowerCase = account.copy(loginName = account.loginName.toLowerCase)

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

                          // find the user that added the external contact
                          val query = Json.obj("contacts.identityId" -> identity.id)
                          Identity.find(query).flatMap {
                            case None => Future(resBadRequest("this user is in nobodies contact book"))
                            case Some(otherIdentity) =>
                              val futureRes: Future[Boolean] = for {
                              // add other identity as contact
                                addContact <- identity.addContact(Contact.create(otherIdentity.id))
                                updateIdentity <- {
                                  // add update identity with details from registration
                                  val set = Map("cameoId" -> account.loginName,
                                    "accountId" -> account.id,
                                    "isDefaultIdentity" -> true,
                                    "displayName" -> additionalValues.displayName.getOrElse(""))
                                  Identity.update(identity.id, IdentityModelUpdate.fromMap(set))
                                }
                                deleteDetails <- {
                                  val deleteValues =
                                    Seq("email", "phoneNumber") ++ {
                                      additionalValues.displayName match {
                                        case None    => Seq("displayName")
                                        case Some(d) => Seq()
                                      }
                                    }
                                  Identity.deleteValues(identity.id, deleteValues).map(_.updatedExisting)
                                }
                              } yield {
                                addContact && updateIdentity && deleteDetails
                              }
                              futureRes.flatMap {
                                case false => Future(resServerError("unable to update identity"))
                                case true =>
                                  // send contact update event to other identity
                                  Identity.find(identity.id).map {
                                    case None => // do nothing
                                    case Some(i) =>
                                      otherIdentity.contacts.find(_.identityId.equals(identity.id)) match {
                                        case None          => // do nothing
                                        case Some(contact) => actors.eventRouter ! ContactUpdate(otherIdentity.id, contact, i)
                                      }
                                  }
                                  createAccountWithIdentity(identity)
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
