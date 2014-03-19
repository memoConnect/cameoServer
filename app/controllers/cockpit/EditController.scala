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
import models.cockpit.CockpitEdit

object EditController {

  def edit(elementName: String, id: String) = Action.async {
    ListController.getEditable(elementName) match {
      case None => Future(resNotFound("entity with name: "+elementName))
      case Some(definition) => definition.getAttributes(id).map {
        case None => resNotFound(elementName + " object with id: " + id)
        case Some(attributes) =>
          val cockpitEdit = new CockpitEdit(id, attributes)
          resOK(cockpitEdit.toJson)
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