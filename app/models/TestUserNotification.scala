package models

import helper.{ IdHelper, MongoCollections }
import play.api.Logger
import play.api.libs.json.{ Format, JsObject, Json, Reads }
import play.modules.reactivemongo.json.collection.JSONCollection
import traits.Model

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 24.06.14
 * Time: 15:07
 */
case class TestUserNotification(id: MongoId,
                                identityId: MongoId,
                                messageType: String,
                                content: String,
                                sender: Boolean,
                                docVersion: Int) {

  def toJson: JsObject = Json.obj("messageType" -> messageType, "content" -> content)
}

object TestUserNotification extends Model[TestUserNotification] {

  def col: JSONCollection = MongoCollections.testUserNotificationCollection

  implicit def mongoFormat: Format[TestUserNotification] = createMongoFormat(Json.reads[TestUserNotification], Json.writes[TestUserNotification])

  def createAndInsert(identityId: MongoId, messageType: String, content: String, sender: Boolean): Future[Boolean] = {
    val msg = new TestUserNotification(IdHelper.generateMongoId(), identityId, messageType, content, sender, docVersion)
    Logger.debug("SAVONG: " + msg)
    MongoCollections.testUserNotificationCollection.insert(msg).map(_.ok)
  }

  def createDefault(): TestUserNotification = TestUserNotification(IdHelper.generateMongoId(), new MongoId(""), "", "", false, docVersion)

  def docVersion: Int = 0

  def evolutions: Map[Int, Reads[JsObject]] = Map()
}
