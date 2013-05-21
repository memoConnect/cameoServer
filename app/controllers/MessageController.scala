package controllers

import play.api.mvc.{Action, Controller}
import play.modules.reactivemongo.MongoController
import play.api.libs.json._
import helper.MongoHelper


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 7:08 PM
 */


object MessageController extends Controller with MongoController with MongoHelper {

  /**
   * Actions
   */

  def sendMessage = Action(parse.json) {
    request =>

      Ok(resOK(JsString("Woo")))


  }




}
