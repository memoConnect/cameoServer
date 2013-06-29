package models

import java.util.Date
import traits.Model
import play.api.libs.json._
import helper.IdHelper

/**
 * User: Björn Reimer
 * Date: 6/29/13
 * Time: 3:04 PM
 */
case class Asset (
                       assetId: String,
                       filesize: String,
                       filename: String,
                       fileType: String,
                       created: Date
                       )

  object Asset extends Model[Asset]
  {

    implicit val collection = conversationCollection
    implicit val mongoFormat: Format[Asset] = createMongoFormat(Json.reads[Asset], Json.writes[Asset])

    // Input/output format for the API
    val inputReads: Reads[Asset] = Json.reads[Asset]

    val outputWrites: Writes[Asset] = Writes {
      asset =>
        Json.obj("assetId" -> asset.assetId) ++
        Json.obj("name" -> asset.filename) ++
        Json.obj("type" -> asset.fileType) ++
        Json.obj("size" -> asset.filesize)
    }

    override val sortWith = {
      (a1: Asset, a2: Asset) => a1.filename < a2.filename
    }
  }