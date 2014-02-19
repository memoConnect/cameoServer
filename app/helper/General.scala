package helper

/**
 * User: Björn Reimer
 * Date: 2/19/14
 * Time: 12:14 PM
 */
object General {

  def safeStringToInt(str: String): Option[Int] = try {
    Some(str.toInt)
  } catch {
    case NumberFormatException => None
  }


}
