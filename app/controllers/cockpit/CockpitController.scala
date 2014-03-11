package controllers.cockpit

import traits.ExtendedController
import play.api.mvc.Action
import models.cockpit.{CockpitElement, CockpitList}

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
      Seq("title1", "title2", "title3", "title4", "title5"),
      Seq(Seq("attr1", "attr2", "attr3", "attr4", "attr5"),
        Seq("attr1", "attr2", "attr3", "attr4", "attr5"),
        Seq("attr1", "attr2", "attr3", "attr4", "attr5"),
        Seq("attr1", "attr2", "attr3", "attr4", "attr5"),
        Seq("attr1", "attr2", "attr3", "attr4", "attr5")))

    Ok(views.html.cockpit.list(list))
  }

  def edit(elementName: String, id: String) = Action {

    val element = new CockpitElement(elementName,
      id,
      Map("title1" -> "content1", "title2" -> "content2", "title3" -> "content3", "title4" -> "content4", "title5" -> "content5")
    )

    Ok(views.html.cockpit.edit(element))
  }

  def modify() = play.mvc.Results.TODO
}
