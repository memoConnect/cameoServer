package helper

import net.sf.uadetector.OperatingSystemFamily
import net.sf.uadetector.service.UADetectorServiceFactory

/**
 * Created by dermicha on 05/11/14.
 */

/**
  * have to be extended based on needs
  * @param userAgent
  */
class UserAgentHelper(userAgent: String) {
  private val uaParser = UADetectorServiceFactory.getResourceModuleParser
  private val parsedUserAgent = uaParser.parse(userAgent)

  def getFamilyName:String = {
    parsedUserAgent.getOperatingSystem.getFamilyName
  }

  def getOsVersion:String = {
    parsedUserAgent.getOperatingSystem.getVersionNumber.toString
  }
}

object UserAgentHelper {
  val osFamilyIos = OperatingSystemFamily.IOS.getName
  val osFamilyAndroid  = OperatingSystemFamily.ANDROID.getName
}

object HTTPHelper {
  val uaParser = UADetectorServiceFactory.getResourceModuleParser

  /**
   * this method could be used to get detailed information based on a HTTP UserAgent String
   * @param userAgent
   * @return a parsed UserAgent
   */
  def parseUserAgent(userAgent: String) = {
    new UserAgentHelper(userAgent)
  }


}
