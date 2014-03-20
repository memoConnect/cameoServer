package helper

import play.api.libs.json.JsObject

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 7:18 PM
 */
object TestHelper {

  private var values: Seq[(String, JsObject)] = Seq()

  def addValue(key: String, value: JsObject) = {
    values = values :+ (key, value)
  }

  def getValues(key: String): Seq[JsObject] = values.filter(_._1.equals(key)).map { _._2 }

  def clear() {
    values = Seq()
  }
}
