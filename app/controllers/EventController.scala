package controllers

import play.api.mvc.Controller
import helper.CmActions.AuthAction
import models.{ MongoId, EventSubscription }
import helper.ResultHelper._
import scala.concurrent.ExecutionContext
import play.api.{ Logger, Play }
import play.api.Play.current
import ExecutionContext.Implicits.global
import play.api.libs.json.Json

/**
 * User: Björn Reimer
 * Date: 09.05.14
 * Time: 13:42
 */
object EventController extends Controller {

  def newSubscription() = AuthAction().async(parse.tolerantJson) {
    request =>
      // check if a secret is used to disable max subscription
      val limitEnabled: Boolean = Play.configuration.getString("events.subscription.debug.secret") match {
        case None             => true
        case Some("disabled") => true
        case Some(str) =>
          // check if there is a secret in the body
          (request.body \ "secret").asOpt[String] match {
            case Some(secret) if secret.equals(str) => false
            case _                                  => true
          }
      }

      // check if maximum number for this user is exceeded
      val max = Play.configuration.getInt("events.subscription.user.limit").get
      EventSubscription.countUserSubscriptions(request.identity.id).map {
        case i if limitEnabled && i >= max =>
          resBadRequest("max number of subscription reached")
        case _ =>
          val subscription = EventSubscription.create(request.identity.id)
          EventSubscription.col.insert(subscription)
          resOk(subscription.toJson)
      }
  }

  def getSubscription(id: String) = AuthAction().async {
    request =>
      EventSubscription.findAndClear(MongoId(id)).map {
        case None => resNotFound("subscription id")
        case Some(subscription) =>
          resOk(subscription.toJson)
      }
  }
}
