package testHelper

import org.specs2.mutable.{BeforeAfter, Before, Specification}
import play.api.Play

/**
 * User: Björn Reimer
 * Date: 3/5/14
 * Time: 11:13 PM
 */
trait StartedApp extends Specification with BeforeAfter {

  override def before = {
    // check if app is started. start it if not
    Play.maybeApplication match {
      case Some(a) =>
      case None =>
        Play.start(Config.app)
    }
  }

  override def after = {}


}
