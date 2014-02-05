package services

/**
 * User: Björn Reimer
 * Date: 11/11/13
 * Time: 9:22 PM
 */
//object Authentication extends Enumeration {
//
// sealed case class UserDef(isAdmin: Boolean, // can perform admin action, this does nothing yet
//                     readMember: Boolean, // can read conversations where the user is a member
//                     writeMember: Boolean, // can write to conversations where the user is a member
//                     createNew: Boolean // can create new conversations
//                     // things like sendSMS, space limit for uploads etc))
//                     ) {
//
//  }
//
//  val ADMIN = UserDef(isAdmin = true, readMember = true, writeMember = true, createNew = true)
//  val USER = UserDef(isAdmin = false, readMember = true, writeMember = true, createNew = true)
//  val ANON = UserDef(isAdmin = false, readMember = true, writeMember = true, createNew = false)
//
//}
object Authentication {

  val AUTH_ADMIN = "admin"
  val AUTH_USER = "user"
  val AUTH_ANON = "anon"

  case class UserClass(
      name: String,
      isAdmin: Boolean, // can perform admin action, this does nothing yet
      accessIfMember: Boolean, // can access conversations (read and write) if the user is a member
      createNew: Boolean, // can create new conversations
      uploadAssets: Boolean // is allowed to upload Assetes
      // things like sendSMS, space limit for uploads etc))
      ) {
    override def toString: String = name
  }

  def getUserClass(name: String): UserClass = {
    // TODO: find a better way to do this, use statics
    name match {
      case AUTH_ADMIN => UserClass(AUTH_ADMIN, isAdmin = true, accessIfMember = true, createNew = true, uploadAssets = true)
      case AUTH_USER  => UserClass(AUTH_USER, isAdmin = false, accessIfMember = true, createNew = true, uploadAssets = true)
      case AUTH_ANON  => UserClass(AUTH_ANON, isAdmin = false, accessIfMember = true, createNew = false, uploadAssets = true)
    }
  }

}

