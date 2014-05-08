package traits

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.concurrent.{ Await, ExecutionContext, Future }
import ExecutionContext.Implicits.global
import models.{Conversation, MongoId}
import play.api.Logger
import helper.JsonHelper._
import reactivemongo.core.commands.LastError
import scala.concurrent.duration._

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 6:46 PM
 */

trait SubModel[A,B] extends Model[A] {

  def parentModel: Model[B]
  def elementName: String
  val col = parentModel.col

  override def find(id: MongoId): Future[Option[A]] = {
    val projection = Json.obj(elementName -> Json.obj("$elemMatch" -> Json.obj("_id" -> id)))
    parentModel.col.find(arrayQuery(elementName, id), projection).one[JsValue].map {
      case None     => None
      case Some(js) => Some((js \ elementName)(0).as[A])
    }
  }

  override def save(js: JsObject): Future[LastError] = {
    val id: MongoId = (js \ "_id").as[MongoId]
    val query = arrayQuery(elementName, id)
    val set = Json.obj("$set" -> Json.obj(elementName + ".$" -> js))
    parentModel.col.update(query, set)
  }

  def delete(parentId: MongoId, id: MongoId): Future[LastError] = {
    val query = Json.obj("_id" -> id)
    val set = Json.obj("$pull" ->
      Json.obj(elementName -> Json.obj("_id" -> id)))
    parentModel.col.update(query, set)
  }

  def delete(parentId: String, id: String): Future[LastError] = delete(new MongoId(parentId),new MongoId(id))

  override def delete(id: MongoId): Future[LastError] = {
    val query = Json.obj(elementName + "._id" -> id)
    val set = Json.obj("$pull" ->
      Json.obj(elementName -> Json.obj("_id" -> id)))
    parentModel.col.update(query, set)
  }
}
