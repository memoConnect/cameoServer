package traits

import play.api.libs.json._
import play.api.libs.json.Reads._
import java.text.SimpleDateFormat
import org.mindrot.jbcrypt.BCrypt
import java.util.{ TimeZone, Date }
import play.modules.reactivemongo.json.collection.JSONCollection
import scala.concurrent.{ ExecutionContext, Future }
import ExecutionContext.Implicits.global
import models.MongoId
import play.api.Logger
import helper.JsonHelper._

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
          // get document version, none == Version 0
          val currentDocVersion = (js \ "docVersion").asOpt[Int].getOrElse(0)
          val readsWithEvolution = getEvolutions(currentDocVersion)
          val newJs: JsObject = js.transform(readsWithEvolution andThen fromMongoDates andThen fromMongoId).get

          // TODO: update in db
          //            col.save(newJs).map {
          //              lastError => if (!lastError.updatedExisting) {
          //                Logger.error("Error applying DB evolution to " + js)
          //              }
          //            }

          JsSuccess(newJs.as[T](reads))
        case _ => JsError()
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
