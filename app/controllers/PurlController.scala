package controllers

import play.api.mvc._
import play.api.libs.json.Json
import traits.{OutputLimits, ExtendedController}
import models.{Conversation, Purl}
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 1/09/13
 * Time: 4:30 PM
 */

object PurlController extends ExtendedController {


  /**
   * Actions
   */
  def getPurl(purl: String, offset: Int, limit: Int) = Action {
    request =>
      Async {
        // define output limits for conversation
        implicit val outputLimits = OutputLimits(offset, limit)

        Purl.find(purl).map {
          case None => NotFound(resKO("The purl does not exist"))
          case Some(purlObject) => {
            Async {
              // get the conversation
              Conversation.find(purlObject.conversationId).map {
                case None => {
                  val er = "The conversation in the purl not exist"
                  Logger.error(er)
                  NotFound(resKO(er))
                }
                case Some(conversation) => {
                  val res = Json.obj("context" -> Purl.toJson(purlObject), "conversation" -> Conversation.toJson
                    (conversation))
                  Ok(resOK(res))
                }
              }
            }
          }
        }
      }
  }


}
