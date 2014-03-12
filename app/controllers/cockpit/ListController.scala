package controllers.cockpit

import play.api.mvc.Action
import play.api.libs.json.{ Reads, Json, JsObject }
import scala.concurrent.{ ExecutionContext, Future }
import models.cockpit.CockpitList
import models.Identity
import org.omg.CosNaming.NamingContextPackage.NotFound
import traits.{ Model, CockpitEditable, ExtendedController }
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

  def getList(name: String): Option[CockpitEditable with Model] = {
    name match {
      case "identity" => Some(Identity)
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
              case Some(obj) =>
                obj.getList(listOptions.limit, listOptions.offset).map { list =>
                  resOK(list.toJson)
                }
            }
          }
      }
  }

  def delete(elementName: String, id: String) = Action.async {

    getList(elementName) match {
      case None => Future(resNotFound("elementName"))
      case Some(obj) => obj.delete(id).map {
        _.updatedExisting match {
          case false => resServerError("could not delete")
          case true  => resOK("deleted")
        }
      }
    }

  }
}
