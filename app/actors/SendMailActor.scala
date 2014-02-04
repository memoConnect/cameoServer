package actors

import akka.actor.Actor
import play.api.{Play, Logger}
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClient
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.simpleemail.model._
import play.api.Play.current
import traits.MongoHelper
import com.amazonaws.{AmazonServiceException, AmazonClientException}
import models.MailMessage
import play.api.Play.current
/**
 * User: Björn Reimer
 * Date: 6/12/13
 * Time: 8:01 PM
 */

class SendMailActor extends Actor with MongoHelper {

  def receive = {
    case (mail: MailMessage) => {

      // add footer to the mail
      Logger.debug("SendMailActor: Sending email to " + mail.to + " from " + mail.from + " with subject \'" + mail.subject + "\'")

      val credentials = new BasicAWSCredentials(Play.configuration.getString("aws.accessKey").getOrElse(""),
        Play.configuration.getString("aws.secretKey").getOrElse(""))
      val client = new AmazonSimpleEmailServiceClient(credentials)

      val sendEmailRequest = new SendEmailRequest()

      val dest = new Destination().withToAddresses(mail.to)
      sendEmailRequest.setDestination(dest)
      sendEmailRequest.setSource(mail.from)
      val awsBody = new Body().withText(new Content().withData(mail.body))
      val awsMessage = new Message().withBody(awsBody).withSubject(new Content().withData(mail.subject))
      sendEmailRequest.setMessage(awsMessage)

      val status = {
        try {
          val result = client.sendEmail(sendEmailRequest)
          "Mail send. Id: " + result.getMessageId
        } catch {
          case ce: AmazonClientException => {
            Logger.error("Error sending mail", ce)
            "Error sending Mail, Could not connect to Amazon"

          }
          case se: AmazonServiceException => {
            Logger.error("Error sending mail", se)
            "Error sending Mail, Could not connect to Amazon"
          }
        }
      }

      Logger.info("SendMailActor: " + status)
    }
  }

}
