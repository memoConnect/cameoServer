package controllers

import play.api.mvc.Action
import play.api.libs.json._
import traits.ExtendedController
import models.{ AccountReservation, Identity, Account }
import reactivemongo.core.errors.DatabaseException
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
import helper.ResultHelper._

/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */
object AccountController extends ExtendedController {

  def checkLogin(login: String): Boolean = {
    login.length >= 6 &&
      login.length < 20 &&
      login.matches("^\\w+$")
  }

  def createAccount = Action.async(parse.tolerantJson) {
    request =>
      val jsBody: JsValue = request.body

      // there might be a reservation secret for reserved accounts
      val reservationSecret: Option[String] = (jsBody \ "reservationSecret").asOpt[String]

      jsBody.validate[Account](Account.createReads).map {
        account =>

          // check if this loginName has been reserved
          val allowed: Future[Boolean] = AccountReservation.checkReserved(account.loginName).map {
            case Some(secret) => if (secret.equals(reservationSecret.getOrElse("fail"))) {
              AccountReservation.deleteReserved(account.loginName)
              true
            }
            else {
              false
            }
            case None => true
          }

          allowed.flatMap {
            case false => Future(Unauthorized(resKO("loginName is reserved, wait 10 min")))
            case true => {
              // create identity and add it to account
              val identity = Identity.create(Some(account.id), account.email, account.phoneNumber)
              Identity.col.insert(identity)
              val account2 = account.copy(identities = Seq(identity.id))

              accountCollection.insert(account2).map {
                lastError =>
                  {
                    if (lastError.ok) {
                      resOK(account2.toJson)
                    }
                    else {
                      InternalServerError(resKO("MongoError: " + lastError))
                    }
                  }
              }.recover {
                // deal with exceptions from duplicate loginNames
                case de: DatabaseException =>
                  if (de.getMessage().contains("loginName")) {
                    BadRequest(resKO("The username already exists"))
                  }
                  else {
                    BadRequest(resKO("Error: " + de.getMessage()))
                  }
                case e => InternalServerError(resKO("Mongo Error: " + e.toString))
              }
            }
          }
      }.recoverTotal(error => Future.successful(BadRequest(resKO(JsError.toFlatJson(error)))))
  }

  def getAccount(loginName: String) = Action.async {
    request =>
      Account.findByLoginName(loginName).map {
        case None          => NotFound(resKO("Account not found: " + loginName))
        case Some(account) => resOK(account.toJson)
      }
  }

  def checkLoginName = Action.async(parse.tolerantJson) {
    request =>
      case class VerifyRequest(loginName: String)

      val reads = (__ \ 'loginName).read[String].map {
        l => VerifyRequest(l)
      }

      request.body.validate[VerifyRequest](reads).map {
        vr =>
          if (checkLogin(vr.loginName)) {
            Account.findByLoginName(vr.loginName).flatMap {
              case Some(a) => Account.findAlternative(vr.loginName).flatMap {
                newLoginName =>
                  {
                    AccountReservation.reserve(newLoginName).map {
                      res => BadRequest(resKO(Json.obj("alternative" -> res.toJson)))
                    }
                  }
              }
              case None => AccountReservation.checkReserved(vr.loginName).flatMap {
                case Some(ra) => Account.findAlternative(vr.loginName).flatMap {
                  newLoginName =>
                    {
                      AccountReservation.reserve(newLoginName).map {
                        res => BadRequest(resKO(Json.obj("alternative" -> res.toJson)))
                      }
                    }
                }
                case None => {
                  AccountReservation.reserve(vr.loginName).map {
                    res =>
                      resOK(res.toJson)
                  }
                }
              }
            }
          }
          else {
            Future(BadRequest(resKO("invalid login name")))
          }
      }.recoverTotal(e => Future(BadRequest(resKO(JsError.toFlatJson(e)))))
  }

  def deleteAccount(loginName: String) = Action.async {
    request =>
      Account.col.remove[JsValue](Json.obj("loginName" -> loginName)).map {
        lastError =>
          if (lastError.updated > 0) {
            resOK(Json.obj("deleted Account" -> loginName))
          }
          else if (lastError.ok) {
            NotFound(resKO("Account not found"))
          }
          else {
            InternalServerError(resKO(lastError.stringify))
          }
      }
  }
}
