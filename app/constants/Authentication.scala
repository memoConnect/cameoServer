package constants

/**
 * User: Björn Reimer
 * Date: 3/13/14
 * Time: 12:23 PM
 */
object Authentication {

  val REQUEST_TOKEN_HEADER_KEY = "Authorization"
  val REQUEST_TWO_FACTOR_TOKEN_HEADER_KEY = "X-TwoFactorToken"

  val REQUEST_TOKEN_MISSING = "no token in header"
  val REQUEST_TWO_FACTOR_TOKEN_MISSING = "no two factor token in header"

  val REQUEST_ACCESS_DENIED = "not allowed"
  val REQUEST_TWO_FACTOR_ACCESS_DENIED = "not allowed"
}
