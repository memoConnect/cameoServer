package traits

import play.api.libs.json._
import play.api.libs.json.Reads._
import java.text.SimpleDateFormat
import org.mindrot.jbcrypt.BCrypt
import java.util.{ TimeZone, Date }
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.concurrent.{ Await, ExecutionContext, Future }
import ExecutionContext.Implicits.global
import models.MongoId
import play.api.Logger
import helper.JsonHelper._
import reactivemongo.core.commands.LastError
import scala.concurrent.duration._

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 6:46 PM
 */
//case class OutputLimits(offset: Int, limit: Int)

trait Model[A] {

  def col: JSONCollection

  def find(id: MongoId): Future[Option[A]] = {
    val query = Json.obj("_id" -> id)
    col.find(query).one[A]
  }

  def find(id: String): Future[Option[A]] = find(new MongoId(id))

  implicit def mongoFormat: Format[A]

  def evolutions: Map[Int, Reads[JsObject]]

  def docVersion: Int

  def save(js: JsObject): Future[LastError] = {
    col.save(js)
  }

  /*
   * Helper functions
   */

  private def createMongoWrites[T](writes: Writes[T]): Writes[T] = Writes {
    obj: T => Json.toJson[T](obj)(writes).transform(toMongoDates andThen toMongoId).getOrElse(Json.obj())
  }

  private def createMongoReads[T](reads: Reads[T]): Reads[T] = Reads {
    js =>
      try {
        js.transform(fromMongoDates andThen fromMongoId).map {
          obj: JsValue => obj.as[T](reads)
        }
      } catch {
        // try to apply evolutions
        case JsResultException(e) =>
          val currentDocVersion = (js \ "docVersion").asOpt[Int].getOrElse(0)
          val readsWithEvolution = getEvolutions(currentDocVersion)

          js.transform(readsWithEvolution).flatMap {
            newJs =>
              // save to db and try to serialise after evolutions
              val futureRes: Future[Boolean] = save(newJs).map { _.ok }
              val res = Await.result(futureRes, 10 minutes)
              res match {
                case false =>
                  Logger.error("Error saving DB evolution: " + newJs); JsError()
                case true =>
                  newJs.asOpt[T](fromMongoDates andThen fromMongoId andThen reads) match {
                    case None =>
                      Logger.error("Error reading Json after evolution"); JsError()
                    case Some(o) => JsSuccess(o)
                  }
              }
          }

        case _: Throwable => JsError()
      }
  }

  def createMongoFormat[T](
    reads: Reads[T],
    writes: Writes[T]) = Format(createMongoReads(reads), createMongoWrites(writes))

  def getEvolutions(fromVersion: Int): Reads[JsObject] = {
    fromVersion match {
      case i if i == docVersion => __.json.pickBranch
      case i if i < docVersion => {
        evolutions(i) andThen getEvolutions(i + 1)
      }
    }
  }

}
