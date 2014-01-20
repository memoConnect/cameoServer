package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.Date
import traits.{OutputLimits, Model}
import play.api.libs.json.Reads._
import scala.concurrent.Future
import helper.IdHelper

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:19 PM
 */


case class Identity(
                     id: MongoId,
                     displayName: Option[String],
                     userKey: String,
                     contacts: Seq[Contact],
                     conversations: Seq[MongoId],
                     assets: Seq[Asset],
                     created: Date,
                     lastUpdated: Date
                     ) {
  def toJson: JsValue = Json.toJson(this)(Identity.outputWrites)

}


object Identity extends Model[Identity] {

  implicit def col = identityCollection

  implicit val mongoFormat: Format[Identity] = createMongoFormat(Json.reads[Identity], Json.writes[Identity])

  def inputReads: Reads[Identity] = (
    Reads.pure[MongoId](MongoId.create()) and
      (__ \ 'displayName).readNullable[String] and
      Reads.pure[String](IdHelper.generateUserKey()) and
      Reads.pure[Seq[Contact]](Seq()) and
      Reads.pure[Seq[MongoId]](Seq()) and
      Reads.pure[Seq[Asset]](Seq()) and
      Reads.pure[Date](new Date()) and
      Reads.pure[Date](new Date())
    )(Identity.apply _)

  def outputWrites(implicit ol: OutputLimits = OutputLimits(0, 0)): Writes[Identity] = Writes {
    i =>
      Json.obj("id" -> i.toJson) ++
        toJsonOrEmpty("displayName", i.displayName) ++
        Json.obj("userKey" -> i.userKey) ++
        addCreated(i.created) ++
        addLastUpdated(i.lastUpdated)
  }

  def find(id: MongoId): Future[Option[Identity]] = {
    val query = Json.obj("_id" -> id)
    col.find(query).one[Identity]
  }

  def create(): MongoId = {
    val identity = new Identity(
      MongoId.create(),
      None,
      IdHelper.generateUserKey(),
      Seq(),
      Seq(),
      Seq(),
      new Date,
      new Date
    )
    col.insert(identity)
    identity.id
  }
}


