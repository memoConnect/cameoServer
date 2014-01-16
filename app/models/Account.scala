package models

/**
 * User: Björn Reimer
 * Date: 1/16/14
 * Time: 4:19 PM
 */
case class Account(
                    loginName: String,
                    password: String,
                    identities: Seq[String],
                    phoneNumber: Option[String],
                    email: Option[String]
                    )
