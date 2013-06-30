package controllers

import play.api.mvc.MultipartFormData
import traits.ExtendedController
import reactivemongo.api.gridfs.ReadFile
import reactivemongo.bson._
import scala.concurrent.Future
import play.api.libs.json._
import reactivemongo.api.gridfs.GridFS
import helper.IdHelper
import play.api.Logger
import models.{Asset, Message}
import reactivemongo.core.commands.LastError
import scala.Some
import reactivemongo.bson.BSONString
import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
import java.util.Date

/**
 * User: Björn Reimer
 * Date: 5/24/13
 * Time: 9:40 PM
 */
object AssetController extends ExtendedController {

  val gridFS = new GridFS(db)
  gridFS.ensureIndex()


  def uploadAsset(token: String, messageId: String) =
    authenticateGET[MultipartFormData[Future[ReadFile[BSONValue]]]](token, bodyParser = gridFSBodyParser(gridFS)) {
      (username, request) => {
        val futureFile = request.body.files.head.ref
        val assetId = IdHelper.generateAssetId()

        Async {
          // check if the message exist
          Message.find(messageId).map {
              case None => BadRequest(resKO("Message not found"))
              case Some(message) => {
                // add message and asset ids to file
                val futureResult = for {
                  file <- futureFile
                  assetResult <- {
                    val query = BSONDocument("_id" -> file.id)
                    val set = BSONDocument("$set" -> BSONDocument(
                      "messageId" -> messageId,
                      "assetId" -> assetId,
                      "user" -> message.from,
                      "created" -> BSONDateTime((new java.util.Date).getTime)))

                    implicit val fileReader = DefaultReadFileReader
                    gridFS.files.update(query, set)
                  }
                  messageResult <- {
                    // create asset object
                    val asset = new Asset(
                      assetId,
                      String.valueOf(file.chunkSize),
                      file.filename,
                      file.contentType.getOrElse("unknown"),
                      new Date)

                    // add asset to message
                    val query = Json.obj("conversationId" -> message.conversationId,
                      "messages.messageId" -> message.messageId)
                    val set = Json.obj("$push" -> Json.obj("messages.$.assets" -> asset))
                    conversationCollection.update(query, set)
                  }
                } yield (messageResult, assetResult)

                Async {
                  futureResult.map {
                    case (messageResult: LastError, assetResult) => {
                      if (messageResult.inError) {
                        Logger.error("Error updating message: " + messageResult.stringify)
                        InternalServerError(resKO("Error updating message: " + messageResult.stringify))
                      } else {
                        Ok(resOK(Json.obj("assetId" -> assetId)))
                      }
                    }
                  }
                }
              }
            }
          }
        }
    }

  def getAsset(token: String, assetId: String) = authenticateGET(token) {
    (username, request) =>
      Async {
        val file = gridFS.find(BSONDocument("assetId" -> new BSONString(assetId)))
        // Frontend always wants inline
        serve(gridFS, file, CONTENT_DISPOSITION_INLINE)
//        request.getQueryString("inline") match {
//          case Some("true") => serve(gridFS, file, CONTENT_DISPOSITION_INLINE)
//          case _ => serve(gridFS, file)
//        }
      }
  }
}
