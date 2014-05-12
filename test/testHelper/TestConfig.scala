package testHelper

import play.api.test.FakeApplication

/**
 * User: Björn Reimer
 * Date: 3/3/14
 * Time: 4:41 PM
 */

object TestConfig {

  val basePath = "/api/v1"
  val baseCockpitPath = "/api/cockpit/v1"
  val dbName = "cameo_test"

  // valid users in the inital Data: login;password;identityId;token
  // 2VqTftqh;password;g9PWZY7xKNbeCO6LPNnx;hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo
  // BMeSfHXQ;password;N2HKgBdxxnWBGxlYY7Dn;viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ

  // test user on dev.cameo.io
  // r1Zhpq8e;password;NyGAvBnLeR3mLEYdofgf;lFFkssj7gE4uTGSZlPlolp82Ozp3fWnOkQEFYO6k

  // Use the same FakeApplication for all tests, so the mongoConnection does not break
  val eventTimeout = 3
  val additionalConfig = Map("mongodb.db" -> dbName, "events.subscription.expire.period" -> eventTimeout)
  val additionalConfigWithLoggingDisabled = Map("mongodb.db" -> dbName, "logger.application" -> "ERROR", "logger.play" -> "ERROR")

  lazy val app = FakeApplication(additionalConfiguration = additionalConfig)

  val cidExisting = "rQHQZHv4ARDXRmnEzJ92"
  val cidExisting2 = "dLBDYFdfj9ymiTblElmN"
  val cidExistingNonMember = "2GOdNSfdPMavyl95KUah"

  val identityExisting = "g9PWZY7xKNbeCO6LPNnx"
  val identityExisting2 = "N2HKgBdxxnWBGxlYY7Dn"

  val cameoIdExisting = "KG5mSGTY8l3"
  val cameoIdExisting2 ="bwyVeVnCvuO"

  val displayNameExisting = "Moeper"
  val displayNameExisting2 = "Moeper2"

  val tokenExisting = "hUbODA2qkVo2JF7YdEYVXe4NaHd82x6rvxxBxXbo"
  val tokenExisting2 = "viRlhZZ1VDAhqcgrljvfzEXCwKj0B2dyAKw5suFZ"

  val telExisting = "+49123456789"
  val emailExisting = "test@cameo.io"

  val invalidPhoneNumbers = Seq("abcd", "+4912345123451234512345", "", "+!\"§$%&/()=")
  val invalidEmails = Seq("a@a.d", "a@a", "a@a aa.de", "a.de", "123@345.43")

  val genericDisplayName = "generic Display Name"
  val existingContactIdentityId = "DOLkwAfL8zuMsh9JcXy4"

  val purlExtern = "MSaKlj4hJP"
  val purlExtern2 = "PkFWPuCiBB"
  val purlExtern3 = "agirsrEN3j"
  val purlExternIdentitityId = "GhEWGfy3Jqx8BRP1pITO"
  val purlIntern = "V3Ml6hzqX8"
  val purlIntern2 = "u02iLiIeQu"
  val purlConversationId = "OM9QeJ4RfJcdscyo52g4"

  val internalContactId = "RJaMVpSkdhMRXc0DqnfT"
  val internalContactIdentityId = "l4ytByiHOw0iJ0LA2hpz"

  val externalContact2 = "EVFrPIr2oPpyVaUZGQjV"

  val validPhoneNumbers: Seq[(String, String)] =
    Seq(
      (" 0173-12  34dd5678"     , "+4917312345678"),
      ("491234512345"           , "+491234512345"),
      ("(0049)1234512345"       , "+491234512345"),
      ("0123/4512345"           , "+491234512345"),
      ("0123-4512345"           , "+491234512345"),
      (" +17234512345         " , "+17234512345")
    )
  val validEmails: Seq[String] = Seq("a-b.c_d@a-b.c_d.co", "123@345.fo", "123@3-4-5.fo")
}
