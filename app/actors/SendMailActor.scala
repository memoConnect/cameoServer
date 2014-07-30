package actors

import akka.actor.Actor
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpleemail.model._
import com.amazonaws.services.simpleemail.{ AmazonSimpleEmailServiceClient, model }
import com.amazonaws.{ AmazonClientException, AmazonServiceException }
import constants.Messaging._
import models._
import play.api.Play.current
import play.api.libs.json.Json
import play.api.{ Logger, Play }

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */
case class Mail(from: String, to: String, body: String, subject: String)
object Mail { implicit val format = Json.format[Mail] }

case class MailWithPurl(message: models.Message, fromIdentity: Identity, toIdentity: Identity, subject: String, email: String)

class SendMailActor extends Actor {

  def sendMail(mail: Mail): Unit = {

    Logger.info("SendMailActor: Sending email to " + mail.to + " from " + mail.from + " with subject \'" + mail.subject + "\'")

    // check if there are there are credentials in the config
    val accessKey = Play.configuration.getString("aws.accessKey")
    val secretKey = Play.configuration.getString("aws.secretKey")

    accessKey.isEmpty || secretKey.isEmpty match {
      case true =>
        Logger.warn("No AWS credentials")
      case false =>
        val credentials = new BasicAWSCredentials(accessKey.get, secretKey.get)
        val client = new AmazonSimpleEmailServiceClient(credentials)
        client.setEndpoint("email.eu-west-1.amazonaws.com")
        val sendEmailRequest = new SendEmailRequest()
        val dest = new Destination().withToAddresses(mail.to)
        sendEmailRequest.setDestination(dest)
        sendEmailRequest.setSource(mail.from)
        val awsBody = new Body().withText(new Content().withData(mail.body))
        val awsMessage = new model.Message().withBody(awsBody).withSubject(new Content().withData(mail.subject))
        sendEmailRequest.setMessage(awsMessage)

        try {
          val result = client.sendEmail(sendEmailRequest)
          Logger.info("Mail send. Id: " + result.getMessageId)
        } catch {
          case ce: AmazonClientException => {
            Logger.error("ACE", ce)
          }
          case se: AmazonServiceException => {
            Logger.error("ACE", se)
          }
        }
    }

  }

  def receive = {
    case mail: Mail =>
      sendMail(mail)

    case MailWithPurl(message, fromIdentity, toIdentity, subject, email) =>

      // get identity of sender
      val fromName = fromIdentity.displayName.getOrElse(fromIdentity.cameoId)
      val from: String = fromName + "<" + Play.configuration.getString("mail.from").get + ">"
      val mailSubject = "[cameo.io] - " + subject
      val to: String = email
      val body: String = message.plain match {
        case Some(PlainMessagePart(Some(text), _)) => text
        case _                                     => MESSAGE_TEXT_REPLACE_ENCRYPTED
      }

      // create purl
      val purl = Purl.create(message.id, toIdentity.id)
      Purl.col.insert(purl)

      // add footer to mail
      val footer = "\n\n---\nRead entire conversation on cameo.io: " + Play.configuration.getString("shortUrl.address").get + "/p/" + purl.id

      // cut message, so it will fit in the sms with the footer.
      val bodyWithFooter = body + footer

      val mail = new Mail(from, to, bodyWithFooter, mailSubject)

      sendMail(mail)
  }
}

