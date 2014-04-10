package controllers.cockpit

import play.api.mvc.Action
import play.api.mvc.BodyParsers.parse
import scala.concurrent.{ ExecutionContext, Future }
import helper.ResultHelper._
import scala.Some
import ExecutionContext.Implicits.global
import play.api.libs.json.JsObject
import models.cockpit.CockpitEdit
import helper.{AuthAction, TwoFactorAuthAction}
import play.api.Logger

object EditController {

  def getEdit(elementName: String, id: String) = TwoFactorAuthAction.async { request =>
    ListController.checkAccessList(request.identity.accountId) {
      ListController.getEditable(elementName) match {
        case None => Future(resNotFound("entity with name: " + elementName))
        case Some(definition) => definition.getAttributes(id).map {
          case None => resNotFound(elementName + " object with id: " + id)
          case Some(attributes) =>
            val cockpitEdit = new CockpitEdit(id, attributes)
            resOK(cockpitEdit.toJson)
        }
      }
    }
  }

  def modify(elementName: String, id: String) = TwoFactorAuthAction.async(parse.tolerantJson) {
    request =>
      // todo: validate body
      ListController.checkAccessList(request.identity.accountId) {
        ListController.getEditable(elementName) match {
          case None => Future(resNotFound("entity with name: " + elementName))
          case Some(definition) => definition.update(id, request.body.as[JsObject]).map {
            case None        => resBadRequest("invalid element id or update values")
            case Some(false) => resServerError("error saving update")
            case Some(true)  => resOK()
          }
        }
      }
  }
}