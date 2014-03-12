package controllers.cockpit

import play.api.mvc.Action
import play.api.libs.json.{ Reads, Json, JsObject }
import scala.concurrent.{ ExecutionContext, Future }
import models.cockpit.CockpitList
import models.Identity
import org.omg.CosNaming.NamingContextPackage.NotFound
import traits.ExtendedController
import ExecutionContext.Implicits.global
import helper.OutputLimits
import helper.ResultHelper._

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 4:33 PM
 */
object ListController extends ExtendedController {

  case class ListOptions(limit: Int,
                         offset: Int)

  object ListOptions {
    implicit val reads: Reads[ListOptions] = Json.reads[ListOptions]
  }

  def getList(name: String): Option[(Int, Int) => Future[CockpitList]] = {
    name match {
      case "identity" => Some(Identity.getList)
      case _          => None
    }
  }

  def list(elementName: String) = Action.async(parse.tolerantJson) {
    request =>
      validateFuture(request.body, ListOptions.reads) {
        listOptions =>
          {
            getList(elementName) match {
              case None => Future(resNotFound("elementName"))
              case Some(f) =>
                f(listOptions.limit, listOptions.offset).map { list =>
                  resOK(list.toJson)
                }
            }
          }
      }
  }
}
