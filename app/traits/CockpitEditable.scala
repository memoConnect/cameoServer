package traits

import models.cockpit.{CockpitListFilter,CockpitElement, CockpitList, CockpitListElement}
import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.core.commands.LastError
import ExecutionContext.Implicits.global
import controllers.cockpit.ListController.ListOptions

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 2:01 PM
 */

case class CockpitEditableDefinition(name: String,
                                     getList: ListOptions => Future[CockpitList],
                                     delete: (String) => Future[LastError],
                                     create: CockpitListElement, // ToDo change to edit element
                                     getEdit: (String) => Future[Option[CockpitElement]]
                                      )

trait CockpitEditable[A] extends Model[A] {

  def cockpitListMapping(obj: A): (Seq[(String, Option[String])], String)

  def getTitles(obj: A): Seq[String] = cockpitListMapping(obj)._1.map {
    case (key, value) => key
  }

  def toCockpitListElement(obj: A): CockpitListElement = {
    val (mapping, id) = cockpitListMapping(obj)
    val attributes: Map[String, Option[String]] = mapping.filter(_._2.isDefined).map {
      case (key, maybeValue) =>
        val index = getTitles(obj).indexWhere(_.equals(key)).toString
        (index, maybeValue)
    }.toMap
    new CockpitListElement(id, attributes)
  }

  def getList(listOptions: ListOptions): Future[CockpitList]

  def getEdit(id: String): Future[Option[CockpitElement]] = find(id).map {
    maybeObj => maybeObj.map {
      obj =>
        val (mapping, id) = cockpitListMapping(obj)
        new CockpitElement(id, mapping)
    }
  }
}
