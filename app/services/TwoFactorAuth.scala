package services

import actors.Sms
import models.{ Account, Identity, TwoFactorSmsKey }
import play.api.Logger
import play.api.Play.current
import play.api.libs.concurrent.Akka

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 4:28 PM
 */
object TwoFactorAuth {

  def sendNewKey(identity: Identity): Future[Option[String]] = {
    // get phonenumber of account
    identity.accountId match {
      case None => Future(Some("identity is external"))
      case Some(accountId) =>
        Logger.debug("finding key")
        Account.find(accountId).map {
          case None => Some("could not find account")
          case Some(account) =>
            Logger.debug("found key")
            account.phoneNumber match {
              case None => Some("account has no phone number")
              case Some(number) =>
                val key = TwoFactorSmsKey.createAndInsert(identity.id)
                val sendSmsActor = Akka.system.actorOf(actors.SendSmsActorProps)
                Logger.info("Sending Two Factor Auth Key: " + key.toString + " to " + number)
                sendSmsActor ! Sms("CameoAuth", number.value, key.toString)
                None
            }
        }
    }
  }

  def verifyKey(key: String, identity: Identity): Future[Boolean] = {
    TwoFactorSmsKey.find(key).map {
      case None => false
      case Some(twoFactorSmsKey) =>
        // compare identities
        twoFactorSmsKey.identityId.equals(identity.id) match {
          case false => false
          case true =>
            // delete key
            twoFactorSmsKey.delete
            true
        }
    }
  }
}
