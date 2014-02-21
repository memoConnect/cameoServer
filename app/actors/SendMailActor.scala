package actors

import akka.actor.Actor
import play.api.{ Play, Logger }
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClient
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpleemail.model._
import play.api.Play.current
import com.amazonaws.{ AmazonServiceException, AmazonClientException }
import models._
import play.api.Play.current
import helper.JsonHelper._
import constants.Messaging._
import com.amazonaws.services.simpleemail.model.Message
import scala.concurrent.{ ExecutionContext, Future }
import models.MailMessage
import models.Message
import com.amazonaws.services.simpleemail.model
import ExecutionContext.Implicits.global

/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */

class SendMailActor extends Actor {

  def sendMail(mail: MailMessage): MessageStatus = {

    Logger.debug("SendMailActor: Sending email to " + mail.to + " from " + mail.from + " with subject \'" + mail.subject + "\'")

    val credentials = new BasicAWSCredentials(Play.configuration.getString("aws.accessKey").getOrElse(""),
      Play.configuration.getString("aws.secretKey").getOrElse(""))
    val client = new AmazonSimpleEmailServiceClient(credentials)

    val sendEmailRequest = new SendEmailRequest()

    val dest = new Destination().withToAddresses(mail.to)
    sendEmailRequest.setDestination(dest)
    sendEmailRequest.setSource(mail.from)
    val awsBody = new Body().withText(new Content().withData(mail.body))
    val awsMessage = new model.Message().withBody(awsBody).withSubject(new Content().withData(mail.subject))
    sendEmailRequest.setMessage(awsMessage)

    val status = try {
      val result = client.sendEmail(sendEmailRequest)
      new MessageStatus(new MongoId(""), MESSAGE_STATUS_SEND, "Mail send. Id: " + result.getMessageId)
    } catch {
      case ce: AmazonClientException => {
        new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, "Error sending Mail, Could not connect to Amazon")
      }
      case se: AmazonServiceException => {
        new MessageStatus(new MongoId(""), MESSAGE_STATUS_ERROR, "Error sending Mail, Could not connect to Amazon")
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
        // TODO update status of single message
        //message.updateStatus(Seq(ms))
      } else {
        // get identity of sender
        val from: String = Play.configuration.getString("mail.from").get
        val subject = "[cameo.io] - Message from " + fromIdentity.displayName.getOrElse(IDENTITY_DEFAULT_DISPLAY_NAME)
        val to: String = toIdentity.email.get.toString
        val body: String = message.messageBody

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
          // WOO
        } else {
          // try again
          sendMailActor ! (message, fromIdentity, toIdentity, tryCount + 1)
        }
      }
  }
}

