package models

import play.api.libs.json._
import play.api.libs.functional.syntax._
import helper.IdHelper
import traits.Model
import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global
import play.api.mvc.{Results, SimpleResult}
import play.mvc.Result
import helper.ResultHelper._

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 5:53 PM
 */
case class Contact(
                    id: MongoId,
                    groups: Seq[String],
                    identityId: MongoId
                    ) {

  def toJson: Future[JsObject] =
    Identity.find(this.identityId).map {
      case None => Json.obj()
      case Some(identity) =>
        Json.toJson(this)(Contact.outputWrites).as[JsObject] ++
          identity.toJson
    }

  def toJsonResult: Future[SimpleResult] = {
    this.toJson.map(
      js => resOK(js))
  }
}

object Contact extends Model[Contact] {

  //  // custom mongoReads to get identity of this contact from db
  //  val mongoReads: Reads[Contact] = createMongoReads[Contact](
  //    reads = Reads {
  //      js => {
  //
  //        Logger.debug("JSON" + js.toString())
  //        val identity = Identity.find((js \ "identityId").as[MongoId])
  //        val pick = {
  //          (__ \ 'groups).json.pickBranch andThen
  //            (__ \ 'id).json.pickBranch andThen
  //            (__ \ 'identity).json.put(identity)
  //        }
  //        js.transform(pick).map {
  //          c => c.as[Contact](Json.reads[Contact])
  //        }
  //      }
  //    }
  //  )
  //
  //  // extract identity from js and write it to db
  //  val mongoWrites: Writes[Contact] = createMongoWrites[Contact](
  //    Writes {
  //      contact => {
  //        Identity.col.insert(contact.identity)
  //        Json.obj("identityId" -> contact.identity.id) ++
  //          Json.obj("groups" -> contact.groups) ++
  //          Json.obj("id" -> contact.id)
  //      }
  //    })

  implicit val mongoFormat: Format[Contact] = createMongoFormat(Json.reads[Contact], Json.writes[Contact])

  def createReads(identityId: MongoId): Reads[Contact] = (
    Reads.pure[MongoId](IdHelper.generateContactId()) and
      ((__ \ 'groups).read[Seq[String]] or Reads.pure(Seq[String]())) and
      Reads.pure[MongoId](identityId)
    )(Contact.apply _)

  def outputWrites: Writes[Contact] = Writes {
    c =>
      Json.obj("id" -> c.id.toJson) ++
        Json.obj("groups" -> c.groups)
  }


}

