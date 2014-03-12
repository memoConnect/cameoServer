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



  def edit(elementName: String, id: String) = Action {

    val element = new CockpitElement(elementName,
      id,
      Map("title1" -> "content1", "title2" -> "content2", "title3" -> "content3", "title4" -> "content4", "title5" -> "content5")
    )

    Ok("eh")
  }

  def modify() = play.mvc.Results.TODO
}
