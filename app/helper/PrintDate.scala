package helper

import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 2:34 PM
 */
object PrintDate {

  val defaultDateFormat: SimpleDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")
  defaultDateFormat.setTimeZone(TimeZone.getTimeZone("Europe/Berlin"))

  def toString(date: Date) = {
    defaultDateFormat.format(date)
  }



}
