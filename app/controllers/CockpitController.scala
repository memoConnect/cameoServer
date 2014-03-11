package controllers

import traits.ExtendedController
import play.api.mvc.Action

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 11:25 AM
 */
object CockpitController extends ExtendedController {


  def index = Action {
    Ok(views.html.cockpit.index())
  }

}
