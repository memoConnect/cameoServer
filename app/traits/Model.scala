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

  /**
   * Helper
   */

  // TODO: put this somewhere more sensible
  val hashPassword: Reads[String] = Reads[String] {
    js =>
      js.asOpt[String] match {
        case None       => JsError("No password")
        case Some(pass) => JsSuccess({
          val hashed = BCrypt.hashpw(pass, BCrypt.gensalt())
          hashed
        })
      }
  }

}
