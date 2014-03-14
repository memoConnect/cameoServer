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

object EditController {

  def edit(elementName: String, id: String) = Action.async {
    ListController.allEditables.find(_.name.equals(elementName)) match {
      case None => Future(resNotFound("entity with name: "+elementName))
      case Some(definition) => definition.getEdit(id).map {
        maybeElement => maybeElement match {
          case Some(element) => resOK(element.toJson)
          case None => resNotFound(elementName+" object with id: "+id)
        }
      }
    }
  }

  def modify(elementName: String, id: String) = Action.async {
    request =>
      validateFuture(request.body, ListOptions.reads) {
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

  }
}