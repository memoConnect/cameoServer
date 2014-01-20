package controllers

import traits.ExtendedController
import models.{Identity, MongoId}
import play.api.mvc.Action
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 1/20/14
 * Time: 12:07 PM
 */
object IdentityController extends ExtendedController {

  def getIdentity(id: String) = Action.async {

    val mongoId = new MongoId(id)

    Identity.find(mongoId).map {
      case None => NotFound(resKO("Identity not found"))
      case Some(identity) => Ok(resOK(identity.toJson))
    }
  }


}
