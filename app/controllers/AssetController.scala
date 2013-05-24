package controllers

import play.api.mvc.{Action, Controller}
import play.modules.reactivemongo.MongoController
import helper.MongoHelper
import java.io.File

/**
 * User: Björn Reimer
 * Date: 5/24/13
 * Time: 9:40 PM
 */
object AssetController extends Controller with MongoController with MongoHelper{

  def uploadAsset(asset: File) = Action( request =>

    Ok(asset.getPath)
  )
}
