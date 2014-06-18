package models

import java.util.Date
import play.api.libs.json.{ Reads, JsObject, Json, Format }
import helper.JsonHelper
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import traits.SubModel

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
      case Some(i) => Json.obj("identity" -> i.toPublicJson) ++ this.toJson
    }
  }
}

object FriendRequest extends SubModel[FriendRequest, Identity] {

  def parentModel = Identity
  def elementName = "friendRequests"
  override val idName = "identityId"

  implicit val mongoFormat: Format[FriendRequest] = Json.format[FriendRequest]

  override def evolutions: Map[Int, Reads[JsObject]] = Map()

  override def createDefault(): FriendRequest = FriendRequest(new MongoId(""), None, new Date)

  override def docVersion: Int = 0
}
