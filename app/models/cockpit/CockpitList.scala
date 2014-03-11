package models.cockpit

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 11:59 AM
 */
case class CockpitList(titles: Seq[String],
                       elements: Seq[CockpitListElement])

case class CockpitListElement(id: String,
                               attributes: Map[String, Option[String]]
                               ) {

  def getTitles = attributes.keySet.toSeq

  def getAttributeList(titles: Seq[String]): Seq[Option[String]] = {
    titles.map { title =>
      attributes.get(title).get
    }
  }

}
