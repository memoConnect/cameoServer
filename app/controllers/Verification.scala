package controllers

import play.api.mvc.{Controller, Action}
import traits.ExtendedController
import models.Identity

object Verification extends Controller with ExtendedController{
  def sendMessage() = Action(parse.tolerantJson) {

    request => request.body.as[Identity]

  }
}