package traits

import play.api.libs.json.{JsValue, Json}

/**
 * User: Björn Reimer
 * Date: 11/11/13
 * Time: 8:51 PM
 */
trait ResultHelper {
  /**
   * Generate Result
   */
  def resOK() = Json.obj("res" -> "OK")

  def resOK(data: JsValue) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)

  def resOK(data: Seq[JsValue]) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)

  def resKO(error: JsValue) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

  def resKO(data: JsValue,
            error: String) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error) ++ Json.obj("data" -> data)

  def resOK(data: String) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)

  def resKO(error: String) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

}
