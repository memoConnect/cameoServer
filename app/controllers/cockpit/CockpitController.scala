package controllers.cockpit

import traits.{ CockpitEditable, ExtendedController }
import play.api.mvc.Action
import models.cockpit.{CockpitElement, CockpitList}
import play.api.libs.json.{Json, JsObject}
import scala.concurrent.{ExecutionContext, Future}
import models.Identity
import ExecutionContext.Implicits.global


/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 11:25 AM
 */
object CockpitController extends ExtendedController {

  def index = Action {
    Ok("ehh")
  }
}
