package helper

import net.sf.uadetector.service.UADetectorServiceFactory

/**
 * Created by dermicha on 05/11/14.
 */
object HTTPHelper {
  val uaParser = UADetectorServiceFactory.getResourceModuleParser

  /**
   * this method could be used to get detailed information based on a HTTP UserAgent String
   * @param userAgent
   * @return a parsed UserAgent
   */
  def parseUserAgent(userAgent: String) = {
    uaParser.parse(userAgent)
  }



}
