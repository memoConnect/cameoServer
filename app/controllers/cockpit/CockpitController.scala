package controllers.cockpit

import traits.ExtendedController
import play.api.mvc.Action
import models.cockpit.CockpitList

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 11:25 AM
 */
object CockpitController extends ExtendedController {


  def index = Action {
    Ok(views.html.cockpit.index())
  }

  def list(elementName: String) = Action {

    val list = new CockpitList(elementName,
      Seq("title1","title2","title3","title4","title5"),
      Seq())

    Ok(views.html.cockpit.list(list))
  }

}
