package controllers.cockpit

import play.api.mvc.Action
import play.api.mvc.BodyParsers.parse
import scala.concurrent.Future
import helper.ResultHelper._
import scala.Some
import scala.Some

object EditController {

  def edit(elementName: String, id: String) = Action.async {
    allEditables.find(_.name.equals(elementName)) match {
      case None => Future(resNotFound("elementName"))
      case Some(definition) => definition.getList(listOptions.limit, listOptions.offset).map { list =>
        resOK(list.toJson)
      }
    }
  }

  def modify(elementName: String, id: String) = play.mvc.Results.TODO
}