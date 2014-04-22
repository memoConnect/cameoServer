package models

import java.util.Date
import play.api.libs.json.{ JsObject, Reads, Json, Format }
import helper.JsonHelper
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
/**
 * User: Björn Reimer
 * Date: 22.04.14
 * Time: 17:28
 */
case class FriendRequest(identityId: MongoId,
                         message: Option[String],
                         created: Date) {

  def toJson: JsObject =
    Json.obj("identityId" -> this.identityId.toString) ++
      JsonHelper.maybeEmptyString("message", this.message) ++
      JsonHelper.addCreated(this.created)

  def toJsonWithIdentity: Future[JsObject] = {
    Identity.find(this.identityId).map {
      case None    => this.toJson
      case Some(i) => Json.obj("identity" -> i.toPublicSummaryJson) ++ this.toJson
    }
  }
}

object FriendRequest {

  implicit val mongoFormat: Format[FriendRequest] = Json.format[FriendRequest]

}
