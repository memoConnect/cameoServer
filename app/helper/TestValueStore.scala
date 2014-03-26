package helper

import play.api.libs.json.JsObject
import java.util.concurrent.TimeoutException
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 7:18 PM
 */
object TestValueStore {

  private var values: Seq[(String, JsObject)] = Seq()
  private var blocked: Boolean = false

  def start() = {
    if (!blocked) {
      values = Seq()
      blocked = true
    } else {
      var maxWait = 100
      // wait untill block is lifted
      while(blocked && maxWait > 0) {
        Logger.debug("waiting: " + maxWait)
        Thread.sleep(50)
        maxWait -= 1
      }
      if(!blocked) {
        values = Seq()
        blocked = true
      } else {
        throw new TimeoutException("timed out waiting for block to be lifted")
      }
    }
  }

  def stop() {
    values = Seq()
    blocked = false
  }

  def addValue(key: String, value: JsObject) = {
      values = values :+ (key, value)
  }

  def getValues(key: String): Seq[JsObject] = values.filter(_._1.equals(key)).map { _._2 }
}
