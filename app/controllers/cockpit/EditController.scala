package controllers.cockpit

import services.AuthenticationActions
import AuthenticationActions._
import helper.ResultHelper._
import models.cockpit.CockpitEdit
import play.api.libs.json.JsObject
import play.api.mvc.BodyParsers.parse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object EditController {

  def getEdit(elementName: String, id: String) = TwoFactorAuthAction().async { request =>
    ListController.checkAccessList(request.identity.accountId) {
      ListController.getEditable(elementName) match {
        case None => Future(resNotFound("entity with name: " + elementName))
        case Some(definition) => definition.getAttributes(id).map {
          case None => resNotFound(elementName + " object with id: " + id)
          case Some(attributes) =>
            val cockpitEdit = new CockpitEdit(id, attributes)
            resOk(cockpitEdit.toJson)
        }
      }
    }
  }

  def modify(elementName: String, id: String) = TwoFactorAuthAction().async(parse.tolerantJson) {
    request =>
      // todo: validate body
      ListController.checkAccessList(request.identity.accountId) {
        ListController.getEditable(elementName) match {
          case None => Future(resNotFound("entity with name: " + elementName))
          case Some(definition) => definition.update(id, request.body.as[JsObject]).map {
            case None        => resBadRequest("invalid element id or update values")
            case Some(false) => resServerError("error saving update")
            case Some(true)  => resOk()
          }
        }
      }
  }
}