package traits

import helper.ResultHelper._
import play.api.libs.json._
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 6:53 PM
 */

trait ExtendedController extends Controller with MongoController {

  def validate[T](js: JsValue, reads: Reads[T])(action: ((T => Result))): Result = {
    js.validate(reads).map {
      action
    }.recoverTotal {
      error => resBadRequest(JsError.toFlatJson(error))
    }
  }

  def validateFuture[T](js: JsValue, reads: Reads[T])(action: ((T => Future[Result]))): Future[Result] = {
    js.validate(reads).map {
      action
    }.recoverTotal {
      error => Future(resBadRequest(JsError.toFlatJson(error)))
    }
  }

}
