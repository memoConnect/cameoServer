package traits

import models.cockpit.{CockpitList, CockpitListElement}
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 2:01 PM
 */
trait CockpitEditable[A] {

  def toCockpitListElement(obj: A): CockpitListElement

  // TODO filter
  def getList(limit: Int, offset: Int): Future[CockpitList]
}
