package controllers.cockpit

import traits.{ CockpitEditable, ExtendedController }
import play.api.mvc.Action
import models.cockpit.CockpitList
import models.Identity
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json.{ Json, JsObject }
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 11:25 AM
 */
object CockpitController extends ExtendedController {

  def index = Action {
    Ok(views.html.cockpit.index())
  }

  def list(listName: String) = Action.async {

    val context: JsObject = Json.obj("name" -> listName)

    def  getList(name: String): Option[Future[CockpitList]] = {
      name match {
        case "identity" => Some(Identity.getList(5,15))
        case _ => None
      }
    }

    // check if we can handle this list
    getList(listName) match {
      case None => Future(NotFound)
      case Some(futureList) =>
        futureList.map { list =>
          Ok(views.html.cockpit.list(list, context))
        }
    }
  }

}
