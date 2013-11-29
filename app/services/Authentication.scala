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
    // TODO: find a better way to do this
    name match {
      case "admin" => UserClass("admin", isAdmin = true, accessIfMember = true, createNew = true, uploadAssets = true)
      case "user" => UserClass("user", isAdmin = false, accessIfMember = true, createNew = true, uploadAssets = true)
      case "anon" => UserClass("anon", isAdmin = false, accessIfMember = true, createNew = false, uploadAssets = true)
    }
  }

}

