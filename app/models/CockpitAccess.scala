package models

import helper.{ IdHelper, MongoCollections }
import play.api.libs.json._
import traits.Model

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class CockpitAccess(id: MongoId,
                         accountId: MongoId)

object CockpitAccess extends Model[CockpitAccess] {

  val col = MongoCollections.cockpitAccessCollection

  implicit val mongoFormat: Format[CockpitAccess] = createMongoFormat(Json.reads[CockpitAccess], Json.writes[CockpitAccess])

  def evolutions = Map()

  def findByAccountId(id: MongoId): Future[Option[CockpitAccess]] = {
    val query = Json.obj("accountId" -> id)
    col.find(query).one[CockpitAccess]
  }

  override def createDefault(): CockpitAccess =
    new CockpitAccess(IdHelper.generateMongoId(), IdHelper.generateAccountId())
}

