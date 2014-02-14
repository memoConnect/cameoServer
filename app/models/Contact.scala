package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.IdHelper
import traits.Model
import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import play.api.mvc.{ Results, SimpleResult }
import play.mvc.Result
import helper.ResultHelper._
import play.api.Logger
import helper.JsonHelper._

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 5:53 PM
 */
case class Contact(id: MongoId,
                   groups: Seq[String],
                   identityId: MongoId) {

  def toJson: JsObject = Json.toJson(this)(Contact.outputWrites).as[JsObject]

  def toJsonWithIdentity: Future[JsObject] =
    Identity.find(this.identityId).map {
      case None => Json.obj()
      case Some(identity) =>
        Json.toJson(this)(Contact.outputWrites).as[JsObject] ++
          Json.obj("identity" -> identity.toJson)
    }

  def toJsonWithIdentityResult: Future[SimpleResult] = {
    this.toJsonWithIdentity.map(
      js => resOK(js))
  }
}

object Contact extends Model[Contact] {

  implicit val mongoFormat: Format[Contact] = createMongoFormat(Json.reads[Contact], Json.writes[Contact])

  implicit def col = identityCollection

  def createReads(identityId: MongoId): Reads[Contact] = (
    Reads.pure[MongoId](IdHelper.generateContactId()) and
    ((__ \ 'groups).read[Seq[String]] or Reads.pure(Seq[String]())) and
    Reads.pure[MongoId](identityId))(Contact.apply _)

  def outputWrites: Writes[Contact] = Writes {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("groups" -> c.groups) ++
        Json.obj("identityId" -> c.identityId.toJson)
  }
}

