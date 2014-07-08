package traits

import helper.JsonHelper._
import models.MongoId
import play.api.Logger
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.core.commands.LastError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext, Future }

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 6:46 PM
 */

trait Model[A] {

  def col: JSONCollection

  def find(id: MongoId): Future[Option[A]] = {
    val query = Json.obj("_id" -> id)
    find(query)
  }

  def find(id: String): Future[Option[A]] = find(new MongoId(id))

  def find(query: JsObject): Future[Option[A]] = {
    col.find(query).one[A]
  }

  def findJs(query: JsObject): Future[Option[JsObject]] = {
    col.find(query).one[JsObject]
  }

  def findAll(query: JsObject): Future[Seq[A]] = {
    col.find(query).cursor[A].collect[Seq]()
  }

  def delete(id: MongoId): Future[LastError] = {
    val query = Json.obj("_id" -> id)
    col.remove(query)
  }

  def delete(id: String): Future[LastError] = delete(new MongoId(id))

  def deleteOptionalValues(id: MongoId, values: Seq[String]): Future[LastError] = {
    val unsetValues = values.foldLeft(Json.obj())((js, value) => js ++ Json.obj(value -> ""))
    val set = Json.obj("$unset" -> unsetValues)
    val query = Json.obj("_id" -> id)
    val res = col.update(query, set)
    res
  }

  implicit def mongoFormat: Format[A]

  def evolutions: Map[Int, Reads[JsObject]]

  def docVersion: Int

  def save(js: JsObject): Future[LastError] = {
    col.save(js)
  }

  def createDefault(): A

  /*
   * Helper functions
   */
  def createMongoWrites(writes: Writes[A]): Writes[A] = Writes {
    obj: A => Json.toJson[A](obj)(writes).transform(toMongoDates andThen toMongoId).getOrElse(Json.obj())
  }

  def createMongoReads(reads: Reads[A]): Reads[A] = Reads {
    js =>
      try {
        js.transform(fromMongoDates andThen fromMongoId).map {
          obj: JsValue => obj.as[A](reads)
        }
      } catch {
        // try to apply evolutions
        case JsResultException(e) =>

          // get whole object
          val id: MongoId = (js \ "_id").as[MongoId]
          val futureResult = findJs(Json.obj("_id" -> id))

          // unfortunately we need to lock until we find the object...
          Await.result(futureResult, 1.minute) match {
            case None => throw new RuntimeException("could not find object. Original: " + js)
            case Some(all) =>
              val currentDocVersion = (all \ "docVersion").asOpt[Int].getOrElse(0)
              val readsWithEvolution = getEvolutions(currentDocVersion)

              all.transform(readsWithEvolution).flatMap {
                newJs =>
                  // try to serialise after evolutions and save to db
                  newJs.validate[A](fromMongoDates andThen fromMongoId andThen reads).map {
                    o =>
                      val futureRes: Future[Boolean] = save(newJs).map {
                        _.ok
                      }
                      val res = Await.result(futureRes, 10.minutes)
                      res match {
                        case false =>
                          Logger.error("Error saving DB evolution: " + newJs)
                        case true =>
                          Logger.info("DB migration successfull")
                      }
                      o
                  }
              }
          }
      }

  }

  def createMongoFormat(reads: Reads[A], writes: Writes[A]) = Format(createMongoReads(reads), createMongoWrites(writes))

  def getEvolutions(fromVersion: Int): Reads[JsObject] = {
    fromVersion match {
      case i if i == docVersion => __.json.pickBranch
      case i if i < docVersion =>
        evolutions(i) andThen getEvolutions(i + 1)
    }
  }

}
