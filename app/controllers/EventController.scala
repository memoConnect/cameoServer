package controllers

import play.api.mvc.Controller
import helper.CmActions.AuthAction
import models.EventSubscription
import helper.ResultHelper._
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 09.05.14
 * Time: 13:42
 */
object EventController extends Controller {

  def newSubscription() = AuthAction().async {
    request =>
      val subscription = EventSubscription.create(request.identity.id)
      EventSubscription.col.insert(subscription)
      Future(resOK(subscription.toJson))
  }

  def getSubscription(id: String) = AuthAction().async {
    request =>
      EventSubscription.find(id).map{
        case None => resNotFound("subscription id")
        case Some(subscription) => resOK(subscription.toJson)
      }
  }

}
