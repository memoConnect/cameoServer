package models

import helper.{ IdHelper, MongoCollections }
import play.api.libs.json.{ Json, Format, JsObject, Reads }
import play.modules.reactivemongo.json.collection.JSONCollection
import traits.Model

/**
 * User: Björn Reimer
 * Date: 24.06.14
 * Time: 15:07
 */
case class TestUserMessage(id: MongoId,
                           identityId: MongoId,
                           messageType: String,
                           content: String,
                           docVersion: Int)

object TestUserMessage extends Model[TestUserMessage] {

  override def col: JSONCollection = MongoCollections.testUserMessageCollection

  override implicit def mongoFormat: Format[TestUserMessage] = createMongoFormat(Json.reads[TestUserMessage], Json.writes[TestUserMessage])

  override def createDefault(): TestUserMessage = TestUserMessage(IdHelper.generateMongoId(), new MongoId(""), "", "", docVersion)

  override def docVersion: Int = 0

  override def evolutions: Map[Int, Reads[JsObject]] = Map()
}
