package models

import java.util.Date

import helper.JsonHelper
import play.api.Logger
import play.api.libs.json.{ Format, JsObject, Json, Reads }
import traits.SubModel

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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
      JsonHelper.maybeEmptyJson("message", this.message) ++
      JsonHelper.addCreated(this.created)

  def toJsonWithIdentity: Future[JsObject] = {
    Identity.find(this.identityId).map {
      case None    => this.toJson
      case Some(i) => Json.obj("identity" -> i.toPublicJson()) ++ this.toJson
    }
  }

  def toJsonWithIdentity(identity: Identity): JsObject = {
    this.identityId.equals(identity.id) match {
      case false =>
        Logger.error("FriendRequest.toJson: identity does not match"); this.toJson
      case true => Json.obj("identity" -> identity.toPublicJson()) ++ this.toJson
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

}
