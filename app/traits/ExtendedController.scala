package traits

import play.api.libs.json._
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import models.{Purl, Token}


/**
 * User: Björn Reimer
 * Date: 5/21/13
 * Time: 6:53 PM
 */
/**
 * Several Helper functions for interaction with MongoDB *
 */
trait ExtendedController extends Controller with MongoController with MongoHelper {



}
