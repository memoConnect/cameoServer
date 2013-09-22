package traits

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.api.Play.current
import play.modules.reactivemongo.json.collection.JSONCollection

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Akka
import akka.actor.Props
import actors.{SendKolibriActor, SendSMSActor, SendMailActor, SendMessageActor}

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 7:10 PM
 */
trait MongoHelper {

  // TODO : create seperate trait
  lazy val sendMessageActor = Akka.system.actorOf(Props[SendMessageActor], name = "sendMessage")
  lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor], name = "sendMail")
  lazy val sendSMSActor = Akka.system.actorOf(Props[SendSMSActor], name = "sendSMS")
  lazy val sendKolibriActor = Akka.system.actorOf(Props[SendKolibriActor], name = "sendKolibri")

  val mongoDB = ReactiveMongoPlugin.db

  val conversationCollection: JSONCollection = mongoDB.collection[JSONCollection]("conversations")
  val userCollection: JSONCollection = mongoDB.collection[JSONCollection]("users")
  val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("token")
  val testCollection: JSONCollection = mongoDB.collection[JSONCollection]("test")
  val purlCollection: JSONCollection = mongoDB.collection[JSONCollection]("purl")

  val emptyObj = __.json.put(Json.obj())

  // converts dates and ids to mongo format ($date and $oid)
  val toMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created \ '$date).json.copyFrom((__ \ 'created).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated \ '$date).json.copyFrom((__ \ 'lastUpdated).json.pick[JsNumber]) or emptyObj)
  }

  val fromMongoDates: Reads[JsObject] = {
    __.json.update((__ \ 'created).json.copyFrom((__ \ 'created \ '$date).json.pick[JsNumber]) or emptyObj) andThen
      __.json.update((__ \ 'lastUpdated).json.copyFrom((__ \ 'lastUpdated \ '$date).json.pick[JsNumber]) or emptyObj)
  }

  def createMongoReads[T](reads: Reads[T]): Reads[T] = Reads {
    js => js.transform(fromMongoDates).map {
      obj: JsValue => obj.as[T](reads)
    }
  }

  def createMongoWrites[T](writes: Writes[T]): Writes[T] = Writes {
    obj: T => Json.toJson[T](obj)(writes).transform(toMongoDates).getOrElse(Json.obj())
  }

  def createMongoFormat[T](
                            reads: Reads[T],
                            writes: Writes[T]
                            ) = Format(createMongoReads(reads), createMongoWrites(writes))
}
