package traits

import models.MongoId
import play.api.Logger
import play.api.libs.json.Reads._
import play.api.libs.json._
import reactivemongo.core.commands.LastError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 6:46 PM
 */

trait SubModel[A, Parent] extends Model[A] {

  def parentModel: Model[Parent]
  def elementName: String

  val idName: String = "_id"
  val col = parentModel.col

  override def find(id: MongoId): Future[Option[A]] = {
    val query = Json.obj(elementName + "." + idName -> id)
    val projection = Json.obj(elementName -> Json.obj("$elemMatch" -> Json.obj(idName -> id)))
    parentModel.col.find(query, projection).one[JsValue].map {
      case None     => None
      case Some(js) => Some((js \ elementName)(0).as[A])
    }
  }

  // todo: apply projection, right now it does nothing...
  override def find(subQuery: JsObject, projection: JsObject = Json.obj()): Future[Option[A]] = {
    val query = Json.obj(elementName -> subQuery)
    val projection = Json.obj(elementName -> Json.obj("$elemMatch" -> subQuery))
    parentModel.col.find(query, projection).one[JsValue].map {
      case None     => None
      case Some(js) => Some((js \ elementName)(0).as[A])
    }
  }

  override def findJs(id: MongoId): Future[Option[JsObject]] = {
    val query = Json.obj(elementName + "." + idName -> id)
    val projection = Json.obj(elementName -> Json.obj("$elemMatch" -> Json.obj(idName -> id)))
    parentModel.col.find(query, projection).one[JsValue].map {
      case None     => None
      case Some(js) => Some((js \ elementName)(0).as[JsObject])
    }
  }

  def findParent(childId: MongoId)(implicit parentReads: Reads[Parent]): Future[Option[Parent]] = {
    val query = Json.obj(elementName + "." + idName -> childId)
    parentModel.col.find(query).one[Parent]
  }

  def update(parentId: MongoId, updateJs: JsObject, customIdName: String = this.idName): Future[LastError] = {
    val id = (updateJs \ customIdName).as[JsValue]
    val query = Json.obj("_id" -> parentId, (elementName + "." + customIdName) -> id)
    val update = Json.obj("$set" -> Json.obj((elementName + ".$") -> updateJs))
    parentModel.col.update(query, update)
  }

  override def save(js: JsObject): Future[LastError] = {
    val id: MongoId = (js \ idName).as[MongoId]
    val query = Json.obj((elementName + "." + idName, id))
    val set = Json.obj("$set" -> Json.obj(elementName + ".$" -> js))
    parentModel.col.update(query, set)
  }

  // append to seq only if the object is unique
  def appendUnique(parentId: MongoId, appendees: Seq[A]): Future[LastError] = {
    val query = Json.obj("_id" -> parentId)
    val set = Json.obj("$addToSet" -> Json.obj(elementName -> Json.obj("$each" -> appendees)))
    parentModel.col.update(query, set)
  }

  def appendUnique(parentId: MongoId, appendee: A): Future[LastError] = {
    appendUnique(parentId, Seq(appendee))
  }

  def appendOrUpdate(parentId: MongoId, appendee: A, customIdName: String = this.idName): Future[LastError] = {
    // mongodb does not support this operation directly, so we need two steps
    // first we try to update
    update(parentId, Json.toJson(appendee).as[JsObject], customIdName).flatMap {
      lastError =>
        // if we updated something we're good, else we need to add a new element
        lastError.updatedExisting match {
          case true =>
            Future(lastError)
          case false => append(parentId, appendee)
        }
    }
  }

  def append(parentId: MongoId, appendees: Seq[A]): Future[LastError] = {
    val query = Json.obj("_id" -> parentId)
    val set = Json.obj("$push" -> Json.obj(elementName -> Json.obj("$each" -> appendees)))
    parentModel.col.update(query, set)
  }

  def append(parentId: MongoId, appendee: A): Future[LastError] = {
    append(parentId, Seq(appendee))
  }

  def delete(parentId: String, id: String): Future[LastError] = delete(new MongoId(parentId), new MongoId(id))

  def delete(parentId: MongoId, id: MongoId): Future[LastError] = {
    val query = Json.obj(idName -> id)
    deleteAll(parentId, query)
  }

  def deleteAll(parentId: MongoId, query: JsObject): Future[LastError] = {
    val parentQuery = Json.obj("_id" -> parentId)
    val set = Json.obj("$pull" -> Json.obj(elementName -> query))
    parentModel.col.update(parentQuery, set)
  }

  override def delete(id: MongoId): Future[LastError] = {
    val query = Json.obj(elementName + "." + idName -> id)
    val set = Json.obj("$pull" ->
      Json.obj(elementName -> Json.obj(idName -> id)))
    parentModel.col.update(query, set)
  }
}
