package actors

import akka.actor.{ Props, Actor }
import play.api.{ Play, Logger }
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClient
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpleemail.model._
import com.amazonaws.{ AmazonServiceException, AmazonClientException }
import models._
import play.api.Play.current
import constants.Messaging._
import scala.concurrent.ExecutionContext
import models.MailMessage
import com.amazonaws.services.simpleemail.model
import ExecutionContext.Implicits.global
import play.api.libs.concurrent.Akka

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */

class SendMailActor extends Actor {

  def sendMail(mail: MailMessage): MessageStatus = {

    Logger.debug("SendMailActor: Sending email to " + mail.to + " from " + mail.from + " with subject \'" + mail.subject + "\'")

    // check if there are there are credentials in the config
    val accessKey = Play.configuration.getString("aws.accessKey")
    val secretKey = Play.configuration.getString("aws.secretKey")

    val status: MessageStatus = accessKey.isEmpty || secretKey.isEmpty match {
      case true => {
        Logger.warn("No AWS credentials")
        new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, "No Credentials")
      }
      case false => {
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
          new MessageStatus(new MongoId(""), MESSAGE_STATUS_SEND, "Mail send. Id: " + result.getMessageId)
        } catch {
          case ce: AmazonClientException => {
            Logger.error("ACE", ce)
            new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, "Error sending Mail, Could not connect to Amazon")
          }
          case se: AmazonServiceException => {
            Logger.error("ACE", se)
            new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, "Error sending Mail, Could not connect to Amazon")
          }
        }
      }
    }

    Logger.info("SendMailActor: Send Mail. STATUS: " + status)
    status
  }

  def receive = {
    case (mail: MailMessage, tryCount: Int) => {
      if (tryCount > MESSAGE_MAX_TRY_COUNT) {
        Logger.error("Max try count of message reached: " + mail)
      } else {
        sendMail(mail)
      }
    }

    case (message: models.Message, fromIdentity: Identity, toIdentity: Identity, tryCount: Int) =>
      // check how often we tried to send this message
      if (tryCount > MESSAGE_MAX_TRY_COUNT) {
        val ms = new MessageStatus(toIdentity.id, MESSAGE_STATUS_ERROR, "max try count reached")
        message.updateSingleStatus(ms)
      } else {
        // get identity of sender
        val from: String = Play.configuration.getString("mail.from").get
        val subject = "[cameo.io] - Message from " + fromIdentity.displayName.getOrElse(IDENTITY_DEFAULT_DISPLAY_NAME)
        val to: String = toIdentity.email.get.toString
        val body: String = message.body

        // create purl
        val purl = Purl.create(message.id, toIdentity.id)
        Purl.col.insert(purl)

        // add footer to mail
        val footer = "---\nRead entire conversation on cameo.io: " + Play.configuration.getString("shortUrl.address").get + "/p/" + purl.id

        // cut message, so it will fit in the sms with the footer.
        val bodyWithFooter = body + footer

        val mail = new MailMessage(from, to, bodyWithFooter, subject)

        val messageStatus = sendMail(mail)

        if (messageStatus.status.equals(MESSAGE_STATUS_SEND)) {
          message.updateSingleStatus(messageStatus.copy(identityId = toIdentity.id))
        } else {
          // try again
          lazy val sendMailActor = Akka.system.actorOf(Props[SendMailActor])
          sendMailActor ! (message, fromIdentity, toIdentity, tryCount + 1)
        }
      }
  }
}

