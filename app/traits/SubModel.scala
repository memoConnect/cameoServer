package traits

import helper.JsonHelper._
import models.MongoId
import play.api.libs.json.Reads._
import play.api.libs.json._
import reactivemongo.core.commands.LastError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ ExecutionContext, Future }

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 6:46 PM
 */

trait SubModel[A, B] extends Model[A] {

  def parentModel: Model[B]
  def elementName: String

  val idName: String = "_id"
  val col = parentModel.col

  override def find(id: MongoId): Future[Option[A]] = {
    find( Json.obj("_id" -> id))
  }

  override def find(query: JsObject): Future[Option[A]] = {
    val projection = Json.obj(elementName -> Json.obj("$elemMatch" -> query))
    parentModel.col.find(arrayQuery(elementName, query), projection).one[JsValue].map {
      case None     => None
      case Some(js) => Some((js \ elementName)(0).as[A])
    }
  }

  override def findJs(query: JsObject): Future[Option[JsObject]] = {
    val projection = Json.obj(elementName -> Json.obj("$elemMatch" -> query))
    parentModel.col.find(arrayQuery(elementName, query), projection).one[JsValue].map {
      case None     => None
      case Some(js) => Some((js \ elementName)(0).as[JsObject])
    }
  }

  def findParent(id: MongoId)(implicit parentReads: Reads[B]): Future[Option[B]] = {
    val query = Json.obj("_id" -> id)
    parentModel.col.find(arrayQuery(elementName, query)).one[B]
  }

  override def save(js: JsObject): Future[LastError] = {
    val id: MongoId = (js \ "_id").as[MongoId]
    val query = arrayQuery(elementName, id)
    val set = Json.obj("$set" -> Json.obj(elementName + ".$" -> js))
    parentModel.col.update(query, set)
  }

  def appendUnique(parentId: MongoId, appendees: Seq[A]): Future[LastError] = {
    val query = Json.obj("_id" -> parentId)
    val set = Json.obj("$addToSet" -> Json.obj(elementName -> Json.obj("$each" -> appendees)))
    parentModel.col.update(query, set)
  }

  def appendUnique(parentId: MongoId, appendee: A): Future[LastError] = {
    appendUnique(parentId, Seq(appendee))
  }

  def append(parentId: MongoId, appendees: Seq[A]): Future[LastError] = {
    val query = Json.obj("_id" -> parentId)
    val set = Json.obj("$push" -> Json.obj(elementName -> Json.obj("$each" -> appendees)))
    parentModel.col.update(query, set)
  }

  def append(parentId: MongoId, appendee: A): Future[LastError] = {
    append(parentId, Seq(appendee))
  }

  def delete(parentId: MongoId, id: MongoId): Future[LastError] = {
    val query = Json.obj("_id" -> parentId)
    val set = Json.obj("$pull" ->
      Json.obj(elementName -> Json.obj(idName -> id)))
    parentModel.col.update(query, set)
  }

  def delete(parentId: String, id: String): Future[LastError] = delete(new MongoId(parentId), new MongoId(id))

  override def delete(id: MongoId): Future[LastError] = {
    val query = Json.obj(elementName + "." + idName -> id)
    val set = Json.obj("$pull" ->
      Json.obj(elementName -> Json.obj(idName -> id)))
    parentModel.col.update(query, set)
  }
}
