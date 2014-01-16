package models

import traits.{OutputLimits, Model}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import java.util.Date
import reactivemongo.api.indexes.{IndexType, Index}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.Logger
import helper.IdHelper

/**
 * User: Björn Reimer
 * Date: 6/25/13
 * Time: 9:45 PM
 */
case class User(
                 username: String,
                 email: String,
                 password: String,
                 name: Option[String],
                 phonenumber: Option[String],
                 userkey: Option[String],
                 contacts: Seq[Contact],
                 conversations: Seq[String],
                 created: Date,
                 lastUpdated: Date
                 )

object User extends Model[User] {

  // create index to unsure unique usernames and emails
  userCollection.indexesManager.ensure(Index(List("username" -> IndexType.Ascending), unique = true, sparse = true))
  userCollection.indexesManager.ensure(Index(List("email" -> IndexType.Ascending), unique = true, sparse = true))

  implicit val collection = userCollection
  implicit val mongoFormat: Format[User] = createMongoFormat(Json.reads[User], Json.writes[User])


  def inputReads: Reads[User] = (
    (__ \ 'username).read[String] and
      (__ \ 'email).read[String](email) and
      (__ \ 'password).read[String](minLength[String](8) andKeep hashPassword) and
      (__ \ 'name).readNullable[String] and
      (__ \ 'phonenumber).readNullable[String] and
      (__ \ 'userkey).readNullable[String] and //.getOrElse(addUserKey(_username), ???
      Reads.pure[Seq[Contact]](Seq[Contact]()) and
      Reads.pure(Seq[String]()) and
      Reads.pure[Date](new Date) and
      Reads.pure[Date](new Date))(User.apply _)

  def outputWrites(implicit ol: OutputLimits = OutputLimits(0, 0)): Writes[User] = Writes {
    user =>
      Json.obj("username" -> user.username) ++
        Json.obj("email" -> user.email) ++
        toJsonOrEmpty("phonenumber", user.phonenumber) ++
        toJsonOrEmpty("name", user.name) ++
        Json.obj("userkey" -> user.userkey.getOrElse[String](addUserKey(user.username))) ++
        //      Contact.toSortedJsonArray("contacts", user.contacts) ++
        //      Json.obj("conversations" -> JsArray(user.conversations.map(JsString(_)).distinct)) ++
        addCreated(user.created) ++
        addLastUpdated(user.lastUpdated)
  }

  def find(username: String): Future[Option[User]] = {
    val query = Json.obj("username" -> username)
    collection.find(query).one[User]
  }

  // add this conversation to the user object
  def addConversation(conversationId: String, username: String): Future[Boolean] = {
    //get the user
    User.find(username).flatMap {
      user => {
        // check if the user already has this conversation
        if (!user.get.conversations.contains(conversationId)) {
          // add it if he does not have it
          val query = Json.obj("username" -> username)
          val set = Json.obj("$addToSet" -> Json.obj("conversations" -> conversationId))
          Logger.debug("Added conversationId " + conversationId + " to user " + username)
          userCollection.update(query, set).map {
            lastError => lastError.updatedExisting
          }
        }
        else {
          Future(true)
        }
      }
    }
  }

  // add UserKey to user is missing
  def addUserKey(username: String) = {
    val userKey = IdHelper.generateUserKey()

    val query = Json.obj("username" -> username)
    val set = Json.obj("$set" -> Json.obj("userKey" -> userKey))
    userCollection.update(query, set)

    userKey
  }
}
