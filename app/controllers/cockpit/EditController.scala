package controllers.cockpit

import play.api.mvc.Action
import play.api.mvc.BodyParsers.parse
import scala.concurrent.{ ExecutionContext, Future }
import helper.ResultHelper._
import scala.Some
import scala.Some
import ExecutionContext.Implicits.global
import helper.TwoFactorAuthAction
import controllers.cockpit.ListController.ListOptions
import play.api.libs.json.{JsValue, Json, Reads}
import play.api.Logger

object EditController {

  def edit(elementName: String, id: String) = Action.async {
    ListController.getEditable(elementName) match {
      case None => Future(resNotFound("entity with name: "+elementName))
      case Some(definition) => definition.getAttributes(id).map {
        case Some(attributes) => resOK(attributes)
        case None => resNotFound(elementName + " object with id: " + id)
      }
    }
  }

  /*def modify(elementName: String, id: String) = Action.async {
    request =>
      validateFuture(request.body, ElementData.reads) {
        listOptions =>
        {
          allEditables.find(definition => definition.name.equals(elementName)) match {
            case None => Future(resNotFound("elementName"))
            case Some(definition) => definition.getList(listOptions).map { list =>
              resOK(list.toJson)
            }
          }
        }
      }

  }*/
}