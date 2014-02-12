package services

import play.modules.reactivemongo.json.collection.JSONCollection
import helper.MongoHelper._
import play.modules.reactivemongo.json.collection.JSONCollection
import java.io.{ File, FileWriter }
import play.api.libs.json.{ Json, JsObject }
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import play.api.Logger
import scala.io.Source

/**
 * User: Björn Reimer
 * Date: 2/6/14
 * Time: 2:47 PM
 */
object DbAdminUtilities {

  def dumpDb() = {
    val path = "fixtures/dump"

    val collections: Seq[JSONCollection] = Seq(tokenCollection, conversationCollection, accountCollection, reservedAccountCollection, identityCollection, purlCollection)

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
                }
                finally fw.close()
            }
          }
        }
    }
  }

  def loadFixtures() {
    for (file <- new File("fixtures/").listFiles) {

      if (file.getName.endsWith(".json")) {

        Logger.debug("Loading Fixture: " + file.getName)

        val col: JSONCollection = file.getName.replace(".json", "") match {
          case ("conversations") => conversationCollection
          case ("accounts")      => accountCollection
          case ("identities")    => identityCollection
          case ("tokens")        => tokenCollection
          case _                 => throw new IllegalArgumentException("No matching collection for this file: " + file.getName)
        }

        for (line <- Source.fromFile(file).getLines()) {
          val json = Json.parse(line)
          col.insert(json)
        }
      }
    }
  }
}