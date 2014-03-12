package traits

import models.cockpit.{ CockpitList, CockpitListElement }
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 2:01 PM
 */
trait CockpitEditable[A] {

  def getTitles(obj: A): Seq[String] = cockpitListMapping(obj)._1.map { case (key, value) => key }

  def cockpitListMapping(obj: A): (Seq[(String, Option[String])], String)

  def toCockpitListElement(obj: A): CockpitListElement = {
    val (mapping, id) = cockpitListMapping(obj)                        
    val attributes: Map[String, Option[String]] = mapping.filter(_._2.isDefined).map {
      case (key, maybeValue) =>
        val index = getTitles(obj).indexWhere(_.equals(key)).toString
        (index, maybeValue)
    }.toMap
    new CockpitListElement(id, attributes)
  }

  def getList(limit: Int, offset: Int): Future[CockpitList]

}
