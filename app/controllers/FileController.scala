package controllers

import traits.ExtendedController
import play.api.libs.json.Json
import models.{ ChunkMeta, FileChunk, FileMeta }
import helper.Utils
import helper.CmActions.AuthAction
import helper.ResultHelper._
import scala.concurrent.{ ExecutionContext, Future }
import play.api.Play
import ExecutionContext.Implicits.global
import play.api.Play.current

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
                  case true  => resOK(fileMeta.toJson)
                }

              }
          }
      }
  }

  def uploadFileChunks(id: String) = AuthAction().async(parse.tolerantJson(512 * 1024)) {
    request =>
      {
        val chunkIndex = request.headers.get("X-Index")

        val headerInvalid = {
          chunkIndex.isEmpty ||
            Utils.safeStringToInt(chunkIndex.get).isEmpty
        }

        headerInvalid match {
          case true => Future(resBadRequest("header content missing or invalid"))
          case false =>
            validateFuture(request.body, FileChunk.createReads) {
              chunk =>
                // check if the give fileId exists
                FileMeta.find(id).flatMap {
                  case None => Future(resNotFound("file"))
                  case Some(fileMeta) =>

                    // check if actual filesize matches the given filesize
                    val fileSizeGrace = Play.configuration.getInt("files.size.grace.percent").get
                    val totalSize = fileMeta.chunks.map(_.chunkSize).sum
                    //                    Logger.debug("Actual: " + totalSize + " Submitted: " + fileMeta.fileSize + " Allowed: " + fileMeta.fileSize * (1f + fileSizeGrace.toFloat / 100f))

                    totalSize <= fileMeta.fileSize * (1f + fileSizeGrace.toFloat / 100f) match {
                      case false => Future(resBadRequest("actual fileSize is bigger than submitted value. Actual: " + totalSize + " Submitted: " + fileMeta.fileSize))
                      case true =>
                        val futureResult: Future[(Boolean, Boolean)] = for {
                          le1 <- fileMeta.addChunk(new ChunkMeta(chunkIndex.get.toInt, chunk.id, chunk.chunk.size))
                          le2 <- FileChunk.col.insert(chunk)
                        } yield {
                          (le1.ok, le2.ok)
                        }
                        futureResult.map {
                          case (false, false) => resServerError("could not save chunk and metadata")
                          case (true, false)  => resServerError("could not update metadata")
                          case (false, true)  => resServerError("could not save chunk")
                          case (true, true)   => resOK()

                        }
                    }
                }
            }
        }
      }
  }

  def getFile(id: String) = AuthAction(allowExternal = true).async {
    request =>
      FileMeta.find(id).map {
        case None => resNotFound("file")
        case Some(fileMeta) =>
          resOK(fileMeta.toJson)
      }
  }

  def getFileChunk(id: String, chunkIndex: String) = AuthAction(allowExternal = true).async {
    request =>
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
                  FileChunk.find(meta.chunkId).map {
                    case None        => resServerError("unable to retrieve chunk")
                    case Some(chunk) => resOK(chunk.toJson)
                  }
              }
          }
      }
  }
}