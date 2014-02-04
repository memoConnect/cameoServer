package models

/**
 * User: Björn Reimer
 * Date: 2/3/14
 * Time: 7:33 PM
 */

// assumes that all data is verified
case class MailMessage(
                        from: String,
                        to: String,
                        body: String,
                        subject: String
                        )