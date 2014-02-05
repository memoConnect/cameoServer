package helper

import play.api.mvc.{ SimpleResult, Results }
import play.api.mvc.Results._
import play.api.libs.json.{ JsValue, Json }

/**
 * User: Björn Reimer
 * Date: 11/11/13
 * Time: 8:51 PM
 */

// TODO: Implement standard methods for notfound, badrequest, etc
object ResultHelper {
  /**
   * Generate Result
   */
  def resOK(): SimpleResult = Ok(Json.obj("res" -> "OK"))

  def resOK(data: JsValue): SimpleResult = Ok(Json.obj("res" -> "OK") ++ Json.obj("data" -> data))

  def resOK(data: Seq[JsValue]): SimpleResult = Ok(Json.obj("res" -> "OK") ++ Json.obj("data" -> data))

  def resOK(data: String): SimpleResult = Ok(Json.obj("res" -> "OK") ++ Json.obj("data" -> data))

  def resKO(error: JsValue) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

  def resKO(data: JsValue,
            error: String) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error) ++ Json.obj("data" -> data)

  def resKO(error: String) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

}
