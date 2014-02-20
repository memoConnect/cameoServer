package models

import play.api.libs.json.{Json, Format}

/**
 * User: Björn Reimer
 * Date: 2/20/14
 * Time: 12:10 PM
 */

case class GlobalState(dbVersion: Int,
                        migrating: Boolean)

object GlobalState {

  implicit val mongoFormat: Format[GlobalState] = Json.format[GlobalState]

}
