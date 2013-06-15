package controllers

import play.api.mvc.MultipartFormData
import traits.ExtendedController
import reactivemongo.api.gridfs.ReadFile
import reactivemongo.bson._
import scala.concurrent.Future
import play.api.libs.json.{JsString, Json}
import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
import reactivemongo.api.gridfs.GridFS
import helper.IdHelper

/**
 * User: Björn Reimer
 * Date: 5/24/13
 * Time: 9:40 PM
 */
object AssetController extends ExtendedController {

  val gridFS = new GridFS(db)
  gridFS.ensureIndex()


  def uploadAsset(token: String, messageId: String) = authenticateGET[MultipartFormData[Future[ReadFile[BSONValue]]]](token, bodyParser = gridFSBodyParser(gridFS)) {
    (username, request) => {
      val futureFile = request.body.files.head.ref
      val assetId = IdHelper.generateAssetId()

      // check if the message exist
      Async {
        findMessage(messageId).map {
          case None => BadRequest(resKO("Message not found"))
          case Some(message) => {
            // add message and asset ids to file
            val futureUpdate = for {file <- futureFile
              updateResult <- {
                val res = gridFS.files.update(BSONDocument("_id" -> file.id), BSONDocument("$set" -> BSONDocument("messageId" -> messageId, "assetId" -> assetId)))

                // create asset object
                val jsAsset = Json.obj("name" -> file.filename, "size" -> JsString(String.valueOf(file.chunkSize)), "type" -> file.contentType, "assetId" -> assetId)

                // add asset to message
                val set = Json.obj("$set" -> Json.obj("messages." + messageId + ".assets." + assetId -> jsAsset))
                conversationCollection.update(getConversationId(message), set).map {
                  lastError => if (lastError.inError) {
                    InternalServerError(resKO("Error updating message: " + lastError.stringify))
                  }
                }
                res
              }} yield updateResult

            Async {
              futureUpdate.map {
                case _ => Ok(resOK(Json.obj("assetId" -> assetId)))
              }.recover {
                case _ => InternalServerError(resKO("Error saving file to gridFS"))
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
        request.getQueryString("inline") match {
          case Some("true") => serve(gridFS, file, CONTENT_DISPOSITION_INLINE)
          case _ => serve(gridFS, file)
        }
      }
  }
}
