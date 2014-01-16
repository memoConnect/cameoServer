package models

import traits.{OutputLimits, Model}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import reactivemongo.api.indexes.{IndexType, Index}
import java.util.Date

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:19 PM
 */
case class Account(
                    id: MongoId,
                    loginName: String,
                    password: String,
                    identities: Seq[MongoId],
                    phoneNumber: Option[String],
                    email: Option[String],
                    created: Date,
                    lastUpdated: Date
                    )

object Account extends Model[Account] {

  val collection = accountCollection

  userCollection.indexesManager.ensure(Index(List("loginName" -> IndexType.Ascending), unique = true, sparse = true))

  implicit val mongoFormat: Format[Account] = createMongoFormat(Json.reads[Account], Json.writes[Account])

  def inputReads: Reads[Account] = (
    Reads.pure[MongoId](MongoId.create()) and
      (__ \ 'loginName).read[String] and
      (__ \ 'password).read[String](minLength[String](8) andKeep hashPassword) and
      Reads.pure[Seq[MongoId]](Seq()) and
      (__ \ 'phoneNumber).readNullable[String] and
      (__ \ 'email).readNullable[String] and
      Reads.pure[Date](new Date()) and
      Reads.pure[Date](new Date())
    )(Account.apply _)

  def outputWrites(implicit ol: OutputLimits = OutputLimits(0, 0)): Writes[Account] = Writes {
    a =>
      Json.obj("loginName" -> a.loginName) ++
        Json.obj("identities" -> a.identities.map(id => id.toJson)) ++
        toJsonOrEmpty("phoneNumber", a.phoneNumber) ++
        toJsonOrEmpty("email", a.email) ++
        addCreated(a.created) ++
        addLastUpdated(a.lastUpdated)
  }
}
