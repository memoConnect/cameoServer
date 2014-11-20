package helper

import java.io.{ File, FileWriter }
import java.util.Date

import helper.MongoCollections._
import models._
import play.api.Play.current
import play.api.libs.iteratee.Iteratee
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.{ Logger, Play }
import play.modules.reactivemongo.ReactiveMongoPlugin
import play.modules.reactivemongo.json.collection.JSONCollection
import services.AvatarGenerator

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.io.Source

/**
 * User: Björn Reimer
 * Date: 2/6/14
 * Time: 2:47 PM
 */
object DbUtilities {

  val collections: Seq[JSONCollection] = Seq(
    conversationCollection,
    accountCollection,
    reservedAccountCollection,
    identityCollection,
    purlCollection,
    globalStateCollection,
    fileMetaCollection,
    verificationCollection,
    twoFactorTokenCollection,
    twoFactorSmsKeyCollection,
    cockpitAccessCollection
  ) :+ ReactiveMongoPlugin.db.collection[JSONCollection](fileChunkCollection.name)

  val minMongoVersion = "2.6"
  var mongoVersion = "na"

  def findColByName(name: String): Option[JSONCollection] = {
    collections.find(_.name.equals(name))
  }

  def dumpDb() = {
    val path = "fixtures/dump"

    collections.map {
      col =>
        {
          col.find(Json.obj()).cursor[JsObject].collect[List](1000, stopOnError = false).map {
            list =>
              Logger.debug("Dumping: " + col.name)
              val fw = new FileWriter(path + "/" + col.name + ".json", false)
              try {
                list.seq.foreach {
                  js =>
                    fw.write(js.toString + "\n")
                }
              } finally fw.close()
          }
        }
    }
  }

  def loadFixtures(): Future[Boolean] = {

    val allResults: Seq[Future[Boolean]] = new File(Play.application.path.getAbsolutePath + "/fixtures/").listFiles.toSeq.map {
      file =>

        if (file.getName.endsWith(".json")) {

          Logger.debug("Loading Fixture: " + file.getName)

          val colName = file.getName.replace(".json", "")

          findColByName(colName) match {
            case None => Future(false)
            case Some(col) =>

              val futureResults: Seq[Future[Boolean]] = Source.fromFile(file).getLines().toSeq.map {
                line =>
                  val json = Json.parse(line)
                  col.insert(json).map(_.ok)
              }

              Future.sequence(futureResults).map {
                _.forall(p => p)
              }
          }
        } else Future(true)
    }

    Future.sequence(allResults).map {
      _.forall(p => p)
    }
  }

  def migrateAll(): Unit = {

    def migrate[A](col: JSONCollection)(implicit reads: Reads[A]) = {
      val enumerator = col.find(Json.obj()).cursor[A].enumerate()
      val iteratee: Iteratee[A, Unit] = Iteratee.foreach {
        model => // do nothing
      }
      enumerator.run(iteratee)
    }

    migrate[Account](Account.col)
    migrate[Conversation](Conversation.col)
    migrate[EventSubscription](EventSubscription.col)
    migrate[FileMeta](FileMeta.col)
    migrate[Identity](Identity.col)
    migrate[Token](Token.col)

  }

  val latestDbVersion = 9

  def migrate(currentVersion: Int): Future[Boolean] = {

    // set global state to migrating
    val query = Json.obj()
    val set = Json.obj("$set" -> Json.obj("migrating" -> true))

    val doUpdate: Future[Boolean] = globalStateCollection.update(query, set).map {
      _.updatedExisting
    }

    val res = doUpdate.flatMap {
      case false =>
        Logger.info("not migrating, since migrating flag is already set to true in global State"); Future(false)
      case true =>
        val res: Seq[Future[Boolean]] = Seq.range[Int](currentVersion, latestDbVersion).seq.map {
          i =>
            Logger.info("migrating version " + i + " (latestVersion: " + latestDbVersion + ")")
            migrations.get(i) match {
              case None =>
                Logger.error("no migration found for version " + i); Future(false)
              case Some(migrationFunction) => migrationFunction("foo")
            }
        }
        Future.sequence(res).map(_.forall(b => b))
    }

    res.map {
      res =>
        Logger.debug("finished migration: " + res)
        val set2 = Json.obj("$set" -> new GlobalState(latestDbVersion, false))
        globalStateCollection.update(query, set2)
        res
    }
  }

  def migrateTokensWithIteratee: Any => Future[Boolean] = foo => {
    Logger.info("migrating tokens")

    def addTokensToIdentity: (JsObject => Boolean) = {
      js =>
        // get identityId
        val id = (js \ "_id").as[MongoId]

        // find all tokens with this identityId
        val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("tokens")
        val query = Json.obj("identityId" -> id)
        val futureTokens: Future[Seq[JsObject]] = tokenCollection.find(query).cursor[JsObject].collect[Seq]()

        // remove identityIds from token
        val removeIdentityId: Reads[JsObject] = (__ \ 'identityId).json.prune
        val futureTokensWithoutId: Future[Seq[JsObject]] = futureTokens.map(_.map(_.transform(removeIdentityId).get))

        // update identity
        val res = futureTokensWithoutId.flatMap {
          tokens =>
            val query2 = Json.obj("_id" -> id)
            val set = Json.obj("$set" -> Json.obj("tokens" -> tokens))
            if (tokens.length > 0)
              identityCollection.update(query2, set).map(_.updatedExisting)
            else
              Future(true)
        }

        val lastRes = Await.result(res, 5.minutes)
        Logger.debug("Migrated Token: " + id)
        lastRes
    }

    val enumerator = identityCollection.find(Json.obj()).cursor[JsObject].enumerate()

    val iteratee: Iteratee[JsObject, Boolean] = Iteratee.fold(true) {
      (result, js) => addTokensToIdentity(js) && result
    }

    enumerator.run(iteratee)
  }

  def migrateRecipients: Any => Future[Boolean] = foo => {
    Logger.info("migrating Recipients")

    def addEncryptedKeysToRecipients: (JsObject => Boolean) =
      js => {
        val id = (js \ "_id").as[MongoId]

        Logger.debug("Starting Migration of Recipients in Conversation: " + id)

        val identityIds: Seq[JsObject] = (js \ "recipients").as[Seq[JsObject]]

        val res = identityIds.length match {
          case i if i == 0 => Future(true)
          case _ =>
            val newRecipients = identityIds.distinct.map {
              id => Json.obj("identityId" -> id)
            }
            val query = Json.obj("_id" -> id)
            val set = Json.obj("$set" -> Json.obj("recipients" -> newRecipients))
            conversationCollection.update(query, set).map {
              _.updatedExisting
            }
        }

        val lastRes = Await.result(res, 5.minutes)
        Logger.debug("Migrated Recipients in Conversation: " + id)
        lastRes
      }

    val enumerator = conversationCollection.find(Json.obj()).cursor[JsObject].enumerate()

    val iteratee: Iteratee[JsObject, Boolean] = Iteratee.fold(true) {
      (result, js) => addEncryptedKeysToRecipients(js) && result
    }

    enumerator.run(iteratee)
  }

  def loginNamesToLowerCase: Any => Future[Boolean] = foo => {
    Logger.info("migrating loginNames")

    def processAccount: (JsObject => Future[Boolean]) =
      js => {
        val id = (js \ "_id").as[MongoId]
        val loginName = (js \ "loginName").as[String]
        val loginNameLower = loginName.toLowerCase

        loginNameLower match {
          case `loginName` =>
            Future(true)
          case x =>
            Logger.debug(loginName + " => " + loginNameLower)
            val query = Json.obj("_id" -> id)
            val set = Json.obj("$set" -> Json.obj("loginName" -> loginNameLower))
            accountCollection.update(query, set).map {
              le =>
                {
                  Logger.debug("updated: " + loginNameLower)
                  le.updatedExisting
                }
            }
        }
      }

    val enumerator = accountCollection.find(Json.obj()).cursor[JsObject].enumerate()

    val iteratee: Iteratee[JsObject, Boolean] = Iteratee.foldM(true) {
      (result, js) => processAccount(js).map(r => r && result)
    }

    enumerator.run(iteratee)

  }

  def addAvatars: Any => Future[Boolean] = foo => {
    Logger.info("adding Avatars")

    val enumerator = identityCollection.find(Json.obj()).cursor[Identity].enumerate()

    val iteratee: Iteratee[Identity, Boolean] = Iteratee.foldM(true) {
      (result, identity) => AvatarGenerator.generate(identity).map(r => r.isDefined && result)
    }

    enumerator.run(iteratee)
  }

  def setDefaultIdentity: Any => Future[Boolean] = foo => {
    Logger.info("setting as default identity")

    val enumerator = identityCollection.find(Json.obj()).cursor[Identity].enumerate()

    val iteratee: Iteratee[Identity, Boolean] = Iteratee.foldM(true) {
      (result, identity) => Future(identity.isDefaultIdentity && result)
    }
    enumerator.run(iteratee)
  }

  def reverseConversations: Any => Future[Boolean] = foo => {
    Logger.info("reversing conversations")

    def processConversation: (JsObject => Future[Boolean]) =
      js => {
        val id = (js \ "_id").as[MongoId]
        val messages = (js \ "messages").as[Seq[JsObject]]

        val query = Json.obj("_id" -> id)
        val set = Json.obj("$set" ->
          Json.obj(
            "messages" -> messages.reverse,
            "numberOfMessages" -> messages.length
          )
        )
        conversationCollection.update(query, set).map {
          le =>
            Logger.debug("Reversed conversation: " + id)
            le.updatedExisting
        }
      }

    val enumerator = conversationCollection.find(Json.obj()).cursor[JsObject].enumerate()

    val iteratee: Iteratee[JsObject, Boolean] = Iteratee.foldM(true) {
      (result, js) => processConversation(js).map(r => r && result)
    }

    enumerator.run(iteratee)
  }

  def clearScaleCache: Any => Future[Boolean] = foo => {
    Logger.info("clearing scale cache")

    // drop file cache dbs
    mongoDB.collection[JSONCollection]("scaleCache.files").drop()
    mongoDB.collection[JSONCollection]("scaleCache.chunks").drop()

    def processFileMeta: (FileMeta => Future[Boolean]) = {
      fileMeta =>
        val cleared = fileMeta.copy(scaleCache = Map())
        FileMeta.save(Json.toJson(cleared).as[JsObject]).map(_.updatedExisting)
    }

    val enumerator = fileMetaCollection.find(Json.obj()).cursor[FileMeta].enumerate()

    val iteratee: Iteratee[FileMeta, Boolean] = Iteratee.foldM(true) {
      (result, fm) => processFileMeta(fm).map(r => r && result)
    }

    enumerator.run(iteratee)
  }

  def setAvatarOwnership: Any => Future[Boolean] = foo => {
    Logger.info("setting avatar ownership")

    // go through all identities and set the ownership of their avatar
    def processIdentity: (Identity => Future[Boolean]) = {
      identity =>
        identity.avatar match {
          case None => Future(true)
          case Some(fileId) =>
            Logger.debug("Setting ownership of: " + fileId)
            FileMeta.setOwner(fileId, identity.id)
        }
    }

    val enumerator = identityCollection.find(Json.obj()).cursor[Identity].enumerate()

    val iteratee: Iteratee[Identity, Boolean] = Iteratee.foldM(true) {
      (result, i) => processIdentity(i).map(r => r && result)
    }

    enumerator.run(iteratee)
  }

  def addLastMessageRead: Any => Future[Boolean] = foo => {
    Logger.info("added last message read to conversations")

    val start = System.currentTimeMillis()

    def processConversation: (Conversation => Future[Boolean]) = {
      conversation =>
        // get messageId of last message
        val updatedRecipients = conversation.recipients.map {
          _.copy(messagesRead = Some(conversation.numberOfMessages))
        }
        val update = Json.obj("$set" -> Json.obj("recipients" -> updatedRecipients))
        Conversation.update(conversation.id, update)
    }

    val enumerator = conversationCollection.find(Json.obj()).cursor[Conversation].enumerate()

    val iteratee: Iteratee[Conversation, Boolean] = Iteratee.foldM(true) {
      (result, c) => processConversation(c).map(r => r && result)
    }

    val res = enumerator.run(iteratee)

    val duration = System.currentTimeMillis() - start
    Logger.info("Duration: " + duration + " milliseconds ")

    res
  }

  def migrations: Map[Int, Any => Future[Boolean]] = Map(
    0 -> migrateTokensWithIteratee,
    1 -> migrateRecipients,
    2 -> loginNamesToLowerCase,
    3 -> addAvatars,
    4 -> setDefaultIdentity,
    5 -> reverseConversations,
    6 -> clearScaleCache,
    7 -> setAvatarOwnership,
    8 -> addLastMessageRead
  )
}