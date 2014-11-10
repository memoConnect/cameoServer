package helper

import net.sf.uadetector.ReadableDeviceCategory.Category
import net.sf.uadetector.{ReadableDeviceCategory, OperatingSystemFamily}
import net.sf.uadetector.service.UADetectorServiceFactory
import play.api.mvc.{AnyContent, Request}

/**
 * Created by dermicha on 05/11/14.
 */
trait OsFamily
case object Ios extends OsFamily
case object Android extends OsFamily


class UserAgentHelper[A](request: Request[A]) {

  private val userAgent = request.headers.get("User-Agent").getOrElse("")
  private val uaParser = UADetectorServiceFactory.getResourceModuleParser
  private val parsedUserAgent = uaParser.parse(userAgent)

  def getFamilyName: Option[OsFamily] = {
    parsedUserAgent.getOperatingSystem.getFamilyName match {
      case str if str.equals(OperatingSystemFamily.IOS.getName) => Some(Ios)
      case str if str.equals(OperatingSystemFamily.ANDROID.getName) => Some(Android)
      case _ => None
    }
  }

  def getOsVersion:String = {
    parsedUserAgent.getOperatingSystem.getVersionNumber.toString
  }

  def isDesktop: Boolean = {
    parsedUserAgent.getDeviceCategory.getCategory.equals(Category.PERSONAL_COMPUTER)
  }
}


