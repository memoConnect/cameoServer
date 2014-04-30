package traits

import play.api.libs.json._
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import models.Purl
import scala.concurrent.Future
import helper.ResultHelper._

/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 6:53 PM
 */

trait ExtendedController extends Controller with MongoController {

  def validate[T](js: JsValue, reads: Reads[T])(action: ((T => SimpleResult))): SimpleResult = {
    js.validate(reads).map {
      action
    }.recoverTotal {
      error => resBadRequest(JsError.toFlatJson(error))
    }
  }

  def validateFuture[T](js: JsValue, reads: Reads[T])(action: ((T => Future[SimpleResult]))): Future[SimpleResult] = {
    js.validate(reads).map {
      action
    }.recoverTotal {
      error => Future.successful(resBadRequest(JsError.toFlatJson(error)))
    }
  }

}
