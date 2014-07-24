package controllers

import actors.NewMessage
import helper.CmActions.AuthAction
import helper.ResultHelper._
import helper.{ IdHelper, Utils }
import models._
import play.api.Play
import play.api.Play.current
import play.api.libs.iteratee.Iteratee
import play.api.libs.json.Json
import play.api.mvc.{ BodyParser, Headers, Result }
import traits.ExtendedController

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.Exception._
import scala.util.control.NonFatal

/**
 * User: Björn Reimer
 * Date: 2/18/14
 * Time: 6:04 PM
 */
object FileController extends ExtendedController {

  def uploadFile = AuthAction().async {
    request =>

      val fileName = request.headers.get("X-File-Name")
      val maxChunks = request.headers.get("X-Max-Chunks")
      val fileSize = request.headers.get("X-File-Size")
      val fileType = request.headers.get("X-File-Type")

      val headerInvalid = {
        fileName.isEmpty ||
          maxChunks.isEmpty ||
          fileSize.isEmpty ||
          fileType.isEmpty ||
          Utils.safeStringToInt(maxChunks.get).isEmpty ||
          Utils.safeStringToInt(fileSize.get).isEmpty
      }

      headerInvalid match {
        case true => Future(resBadRequest("header content missing or invalid"))
        case false =>
          // check filesize
          fileSize.get.toInt <= Play.configuration.getInt("files.size.max").get match {
            case false => Future(resBadRequest(Json.obj("maxFileSize" -> Play.configuration.getInt("files.size.max").get)))
            case true =>

              val fileMeta = FileMeta.create(Seq(), fileName.get, maxChunks.get.toInt, fileSize.get.toInt, fileType.get)
              FileMeta.col.insert(fileMeta).map { lastError =>
                lastError.ok match {
                  case false => resServerError("could not save chunk")
                  case true  => resOk(fileMeta.toJson)
                }
              }
          }
      }
  }

  val binaryToMongoParser: BodyParser[ChunkMeta] = BodyParser("binaryToMongoParser") {

    requestHeader =>
      case object InvalidIndexException extends RuntimeException

      Iteratee.consume[Array[Byte]]().map { bytes =>
        allCatch[ChunkMeta].either {
          requestHeader.headers.get("X-Index").flatMap(Utils.safeStringToInt) match {
            case None => throw InvalidIndexException
            case Some(index) =>
              val chunkMeta = new ChunkMeta(index, IdHelper.generateChunkId, bytes.length)
              FileChunk.insert(chunkMeta.chunkId.id, bytes)
              chunkMeta
          }
        }.left.map {
          case InvalidIndexException =>
            resBadRequest("invalid chunk index")
          case NonFatal(e) =>
            resBadRequest(e.toString)
          case t => throw t
        }
      }
  }

  def uploadFileChunks(id: String) = AuthAction().async(binaryToMongoParser) {
    request =>
      {
        // check if the give fileId exists
        FileMeta.find(id).map {
          case None =>
            FileChunk.delete(request.body.chunkId.id)
            resNotFound("file")
          case Some(fileMeta) =>
            // check if actual filesize matches the given filesize
            val fileSizeGrace = Play.configuration.getInt("files.size.grace.percent").get
            val totalSize = fileMeta.chunks.map(_.chunkSize).sum + request.body.chunkSize
            totalSize <= fileMeta.fileSize * (1f + fileSizeGrace.toFloat / 100f) match {
              case false => resBadRequest("actual fileSize is bigger than submitted value. Actual: " + totalSize + " Submitted: " + fileMeta.fileSize)
              case true =>
                fileMeta.addChunk(request.body)
                resOk()
            }
        }
      }
  }

  // checks if request contains If-None-Match header
  def checkEtag(headers: Headers)(action: => Future[Result]): Future[Result] = {
    headers.get("If-None-Match") match {
      case Some(s) =>
        // always return not modified(304), since files never change (for now)
        Future(resNotModified())
      case None =>
        action
    }
  }

  def getFile(id: String) = AuthAction(allowExternal = true).async {
    request =>
      checkEtag(request.headers) {
        FileMeta.find(id).map {
          case None => resNotFound("file")
          case Some(fileMeta) =>
            resOk(fileMeta.toJson)
        }
      }
  }

  def getFileChunk(id: String, chunkIndex: String) = AuthAction(allowExternal = true).async {
    request =>
      checkEtag(request.headers) {
        Utils.safeStringToInt(chunkIndex) match {
          case None => Future(resBadRequest("chunkIndex is not a number"))
          case Some(i) =>
            // check if file exists
            FileMeta.find(id).flatMap {
              case None => Future(resNotFound("file"))
              case Some(fileMeta) =>
                fileMeta.chunks.find(_.index == i) match {
                  case None => Future(resNotFound("chunk index"))
                  case Some(meta) =>
                    FileChunk.find(meta.chunkId.id).map {
                      case None => resServerError("unable to retrieve chunk")
                      case Some(data) =>
                        resOKWithCache(data, meta.chunkId.toString)
                    }
                }
            }
        }
      }
  }

  case class FileComplete(messageId: Option[String])
  object FileComplete {
    implicit val format = Json.format[FileComplete]
  }

  def uploadFileComplete(id: String) = AuthAction().async(parse.tolerantJson) {
    request =>
      FileMeta.find(id).flatMap {
        case None => Future(resNotFound("file"))
        case Some(fileMeta) =>
          fileMeta.setCompleted(true).flatMap {
            case false => Future(resServerError("unable to update"))
            case true =>
              // check if there is a messageId given, if yes send event to all recipients
              validateFuture(request.body, FileComplete.format) { fc =>
                fc.messageId match {
                  case None => Future(resOk("completed"))
                  case Some(messageId) =>
                    Message.findParent(new MongoId(messageId)).map {
                      case None => resNotFound("messageId")
                      case Some(conversation) =>
                        val message = conversation.messages.find(_.id.id.equals(messageId)).get
                        conversation.recipients.foreach {
                          recipient =>
                            actors.eventRouter ! NewMessage(recipient.identityId, conversation.id, message)
                        }
                        resOk("completed")
                    }
                }
              }
          }
      }
  }
}