package controllers

import play.modules.reactivemongo.MongoController
import play.api.mvc.{Action, Controller}

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import org.mindrot.jbcrypt.BCrypt
import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsNumber
import reactivemongo.bson.BSONObjectID

/**
 * User: Björn Reimer
 * Date: 5/16/13
 * Time: 10:58 AM
 */


object UserController extends Controller with MongoController {
  // connection to collection in mongodb
  //def userCollection: JSONCollection = db.collection[JSONCollection]("user")

  val emptyObj = __.json.put(Json.obj())

  // Validates a user
  def validateUser: Reads[JsObject] = (

    (__ \ 'username).json.pickBranch(Reads.of[JsString]) and
      (__ \ 'email).json.pickBranch(Reads.of[JsString] keepAnd Reads.email) and
      (__ \ 'password).json.pickBranch(Reads.of[JsString] keepAnd Reads.minLength[String](8)) and
      ((__ \ 'name).json.pickBranch(Reads.of[JsString]) or emptyObj) and
      ((__ \ 'phonenumber).json.pickBranch(Reads.of[JsString]) or emptyObj)

    ).reduce

  // hash the password
  def hashPassword: Reads[JsObject] = {
    (__ \ 'password).json.update(of[JsString].map {
      case JsString(pass: String) => JsString(BCrypt.hashpw(pass, BCrypt.gensalt()))
    }
    )
  }

  /// Generate Object ID and creation date
  val generateId = (__ \ '_id \ '$oid).json.put(JsString(BSONObjectID.generate.stringify))
  val generateCreated = (__ \ 'created \ '$date).json.put(JsNumber((new java.util.Date).getTime))
  val addObjectIdAndDate: Reads[JsObject] = __.json.update((generateId and generateCreated).reduce)


  def addUser = Action(parse.json) {
    request =>
      val body: JsValue = request.body

      body.transform(validateUser andThen hashPassword andThen addObjectIdAndDate).map {
        jsRes => Ok(jsRes)
      }.recoverTotal(
        error => BadRequest(JsError.toFlatJson(error))
      )
  }


}
