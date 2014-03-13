package models

import java.util.Date
import traits.{ Model }
import play.api.libs.json._
import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import reactivemongo.api.indexes.{ IndexType, Index }
import helper.JsonHelper._
import helper.{ MongoCollections, IdHelper }
import reactivemongo.core.commands.LastError
import org.joda.time.Interval

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 11:31 AM
 */
case class TwoFactorToken(id: MongoId,
                          identityId: MongoId,
                          created: Date) {
  def toJson: JsValue = Json.toJson(this)(TwoFactorToken.outputWrites)
}

object TwoFactorToken extends Model[TwoFactorToken] {

  val col = MongoCollections.twoFactorTokenCollection

  implicit val mongoFormat: Format[TwoFactorToken] = createMongoFormat(Json.reads[TwoFactorToken], Json.writes[TwoFactorToken])

  def docVersion = 0
  def evolutions = Map()
  def outputWrites = Writes[TwoFactorToken] {
    t =>
      Json.obj("token" -> t.id.toJson) ++
        addCreated(t.created)
  }

//  def create(): TwoFactorToken = {
//    new TwoFactorToken(
//      IdHelper.generateAccessToken(),
//      new Date)
//  }

}

