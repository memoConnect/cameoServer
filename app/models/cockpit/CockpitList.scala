package models.cockpit

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 11:59 AM
 */
case class CockpitList(name: String,
                       titles: Seq[String],
                       elements: Seq[Seq[String]])
