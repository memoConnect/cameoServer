package helper

import play.api.libs.json._
import play.api.libs.json.Reads._
import reactivemongo.bson.BSONObjectID
import play.api.libs.functional.syntax._
import play.api.mvc._
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsNumber
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.MongoController
import scala.concurrent.Future


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 6:53 PM
 */

/**
 * Several Helper functions for interaction with MongoDB *
 */

trait ExtendedController extends Controller with MongoController {

  /**
    * MongoDB Collections
    */

  lazy val conversationCollection: JSONCollection = db.collection[JSONCollection]("conversations")
  lazy val userCollection: JSONCollection = db.collection[JSONCollection]("users")
  lazy val tokenCollection: JSONCollection = db.collection[JSONCollection]("token")

  /**
   * Transformation Helper
   */

  // empty Object
  val emptyObj = __.json.put(Json.obj())

  /// Generate Object ID and creation date
  val generateId = (__ \ '_id \ '$oid).json.put(JsString(BSONObjectID.generate.stringify))
  val generateCreated = (__ \ 'created \ '$date).json.put(JsNumber((new java.util.Date).getTime))

  val addObjectId: Reads[JsObject] = __.json.update(generateId)
  val addCreateDate: Reads[JsObject] = __.json.update(generateCreated)
  val addObjectIdAndDate: Reads[JsObject] = __.json.update((generateId and generateCreated).reduce)

  // generate result
  def resOK(data: JsValue) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)
  def resKO(error: JsValue) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)
  def resOK(data: String) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)
  def resKO(error: String) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

  // convert object id and date between json and bson format
  val toObjectId = Writes[String] {
    s => Json.obj("_id" -> Json.obj("$oid" -> s))
  }
  val fromObjectId = (__ \ 'id).json.copyFrom((__ \ '_id \ '$oid).json.pick)
  val fromCreated = __.json.update((__ \ 'created).json.copyFrom((__ \ 'created \ '$date).json.pick))

  // add status message
  def addStatus(status: String): Reads[JsObject] = __.json.update((__ \ 'status).json.put(JsString(status)))

  /**
    * Authentication
    */

  // checks if the token belongs to the given userclass
  def AuthenticateToken(requireAdminRights: Boolean = false)(f: Request[JsValue] => Result) = {
    Action(parse.tolerantJson) {
      request => {
        (request.body \ "token").asOpt[String] match {
          case None => Unauthorized(resKO("No token"))
          case Some(m) => Async {
            // check if the token is in the database
            val futureUser = tokenCollection.find(Json.obj("token" -> m)).one[JsValue]
            futureUser.map {
              case None => Unauthorized(resKO("Invalid Token"))
              case Some(js) => {
                // we found the token, check if it has the proper rights
                if (requireAdminRights) {
                  (js \ "isAdmin").asOpt[Boolean] match {
                    case None => Unauthorized(resKO("This action requires admin privileges"))
                    case Some(b: Boolean) => {
                      if (!b) Unauthorized(resKO("This action requires admin privileges"))
                      else f(request)
                    }
                  }
                }
                else
                  f(request)
              }
            }
          }
        }
      }
    }
  }
}
