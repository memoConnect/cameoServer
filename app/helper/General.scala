package helper

import java.lang.NumberFormatException
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 2/19/14
 * Time: 12:14 PM
 */
object General {

  def safeStringToInt(str: String): Option[Int] =
    try {
      Some(str.toInt)
    } catch {
      case e: NumberFormatException =>
        Logger.debug("NOT a number: " + str)
        None
    }

}
