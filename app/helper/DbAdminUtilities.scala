package helper

import play.modules.reactivemongo.json.collection.JSONCollection
import helper.JsonHelper._
import play.modules.reactivemongo.json.collection.JSONCollection
import java.io.{ File, FileWriter }
import play.api.libs.json.{ Reads, Json, JsObject }
import scala.concurrent.{Await, Future, ExecutionContext}
import ExecutionContext.Implicits.global
import play.api.Logger
import scala.io.Source
import reactivemongo.core.commands.LastError
import models.{ GlobalState, MongoId, Token }
import play.api.libs.iteratee.Iteratee
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.util.Date
import traits.Model
import play.api.libs.json.Reads._
import scala.concurrent.duration._

/**
 * User: Björn Reimer
 * Date: 2/6/14
 * Time: 2:47 PM
 */
object DbAdminUtilities {

  def dumpDb() = {
    val path = "fixtures/dump"

    val collections: Seq[JSONCollection] = Seq(
      conversationCollection,
      accountCollection,
      reservedAccountCollection,
      identityCollection,
      purlCollection,
      globalStateCollection,
      fileChunkCollection,
      fileMetaCollection
    )

    collections.map {
      col =>
        {
          try {
            col.find(Json.obj()).cursor[JsObject].collect[List](1000, stopOnError = false).map {
              list =>
                Logger.debug("Dumping: " + col.name)
                val fw = new FileWriter(path + "/" + col.name + ".json", false)
                try {
                  list.seq.foreach { js =>
                    fw.write(js.toString + "\n")
                  }
                } finally fw.close()
            }
          }
        }
    }
  }

  def loadFixtures(): Future[Boolean] = {
    val allResults: Seq[Future[Boolean]] = new File("fixtures/").listFiles.toSeq.map {
      file =>

        if (file.getName.endsWith(".json")) {

          Logger.debug("Loading Fixture: " + file.getName)

          lazy val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("tokens")

          val col: JSONCollection = file.getName.replace(".json", "") match {
            case ("conversations") => conversationCollection
            case ("accounts")      => accountCollection
            case ("identities")    => identityCollection
            case ("tokens")        => tokenCollection
            case ("purls")         => purlCollection
            case ("globalState")   => globalStateCollection
            case _                 => throw new IllegalArgumentException("No matching collection for this file: " + file.getName)
          }

          val futureResults: Seq[Future[Boolean]] = Source.fromFile(file).getLines().toSeq.map {
            line =>
              val json = Json.parse(line)
              col.insert(json).map(_.ok)
          }

          Future.sequence(futureResults).map {
            _.forall(p => p)
          }
        } else Future(true)
    }

    Future.sequence(allResults).map {
      _.forall(p => p)
    }
  }

  val latestDbVersion = 1

  def migrate(currentVersion: Int): Future[Boolean] = {

    // set global state to migrating
    val query = Json.obj()
    val set = Json.obj("$set" -> Json.obj("migrating" -> true))

    val doUpdate: Future[Boolean] = globalStateCollection.update(query, set).map { _.updatedExisting }

    val res = doUpdate.flatMap {
      case false =>
        Logger.info("not migrating, since migrating flag is already set to true in global State"); Future(false)
      case true =>
        val res: Seq[Future[Boolean]] = Seq.range[Int](currentVersion, latestDbVersion).map {
          i =>
            Logger.info("migrating version " + i + " (latestVersion: " + latestDbVersion + ")")
            migrations.get(i) match {
              case None =>
                Logger.error("no migration found for version " + i); Future(false)
              case Some(migration) => migration
            }
        }
        Future.sequence(res).map(_.forall(b => b))
    }

    res.map {
      res =>
        val set2 = Json.obj("$set" -> new GlobalState(latestDbVersion, false))
        globalStateCollection.update(query, set2).map { _.updatedExisting }
        res
    }
  }

  // todo find out how to do this with iteratees...
  def migrateTokens: Future[Boolean] = {
    Logger.debug("migrating tokens")

    val addTokensToIdentity: (JsObject => Boolean) = {
      js =>
        // get identityId
        val id = (js \ "_id").as[MongoId]

        // find all tokens with this identityId
        lazy val tokenCollection: JSONCollection = mongoDB.collection[JSONCollection]("tokens")
        val query = Json.obj("identityId" -> id)
        val futureTokens: Future[Seq[JsObject]] = tokenCollection.find(query).cursor[JsObject].collect[Seq]()

        // remove identityIds from token
        val removeIdentityId: Reads[JsObject] = (__ \ 'identityId).json.prune
        val futureTokensWithoutId: Future[Seq[JsObject]] = futureTokens.map(_.map(_.transform(removeIdentityId).get))

        // update identity
        val res = futureTokensWithoutId.flatMap { tokens =>
          val query2 = Json.obj("_id" -> id)
          val set = Json.obj("$set" -> Json.obj("tokens" -> tokens))
          if(tokens.length > 0)
            identityCollection.update(query2, set).map(_.updatedExisting)
          else
            Future(true)
        }

        val res = Await.result(res, 5 minutes)
        Logger.info("Migrated identity: " + id)
        res
    }

    // find all identity
    val allResults: Future[Seq[Boolean]] = identityCollection.find(Json.obj()).cursor[JsObject].collect[Stream]().map {
      // search for all their tokens and them to the identity
      _.seq.map(addTokensToIdentity)
    }

    // get last result
    allResults.map(seq => seq.last)
  }

  def migrations: Map[Int, Future[Boolean]] = Map(0 -> migrateTokens)
}