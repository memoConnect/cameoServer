package models

import traits.Model
import play.api.libs.json._
import reactivemongo.api.indexes.{ IndexType, Index }
import helper.IdHelper
import java.util.Date
import scala.concurrent.{ Future, ExecutionContext }
import ExecutionContext.Implicits.global
import helper.JsonHelper._

/**
 * User: Björn Reimer
 * Date: 9/1/13
 * Time: 5:23 PM
 */

case class Purl(purl: String,
                conversationId: String,
                userType: String,
                recipientId: String,
                username: Option[String],
                name: Option[String],
                token: Option[String]) {
  def toJson: JsObject = Json.toJson(this)(Purl.outputWrites).as[JsObject]

}

object Purl extends Model[Purl] {

  purlCollection.indexesManager.ensure(Index(List("purl" -> IndexType.Ascending), unique = true, sparse = true))

  implicit val col = purlCollection
  implicit val mongoFormat: Format[Purl] = createMongoFormat(Json.reads[Purl], Json.writes[Purl])

  // Input/output format for the API
  def createReads = Json.reads[Purl]

  def outputWrites: Writes[Purl] = Writes {
    purl =>
      Json.obj("conversationId" -> purl.conversationId) ++
        Json.obj("userType" -> purl.userType) ++
        toJsonOrEmpty("username", purl.username) ++
        toJsonOrEmpty("name", purl.name) ++
        toJsonOrEmpty("token", purl.token)
  }

  /*
   * Helper
   */
  //  def createPurl(conversationId: String, recipient: Recipient): String = {
  //    // check if the recipient is another user or not
  //    val purl: Purl = if (recipient.messageType.toLowerCase().equals("otherUser")) {
  //      // we are sending to another user => only need username
  //      new Purl(IdHelper.generatePurl(), conversationId, "registered", recipient.recipientId, Some(recipient.sendTo), None, None)
  //    }
  //    else {
  //      // we are sending to anon user => create new (temporary) token, save Display name
  //      val token = new Token(IdHelper.generateAccessToken(), None, Some(IdHelper.generatePurl()), Some("anon"), new Date)
  //      tokenCollection.insert(token)
  //      new Purl(token.purl.get, conversationId, "unregistered", recipient.recipientId, None, Some(recipient.name), Some(token.token))
  //    }
  //
  //    // write to db and return purl
  //    purlCollection.insert(purl)
  //    purl.purl
  //  }
  //
  //  def createPurl(conversationId: String, user: User): String = {
  //    // TODO get recipientID
  //    val purl = new Purl(IdHelper.generatePurl(), conversationId, "registered", "", Some(user.username), None, None)
  //
  //    // write to db and return purl
  //    purlCollection.insert(purl)
  //    purl.purl
  //  }
}