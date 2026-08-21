package gitbucket.core.util

import gitbucket.core.model.Account
import gitbucket.core.service.SystemSettingsService

import org.apache.commons.mail.{DefaultAuthenticator, HtmlEmail}
import SystemSettingsService.SystemSettings

class Mailer(settings: SystemSettings) {
  private val logger = LoggerFactory.getLogger(getClass)

  def send(
    to: String,
    subject: String,
    textMsg: String,
    htmlMsg: Option[String] = None,
    loginAccount: Option[Account] = None
  ): Unit = {
    logger.debug(">>> Entering send(to={},subject={},textMsg={},htmlMsg={},loginAccount={})", to, subject, textMsg, htmlMsg, loginAccount)
    createMail(subject, textMsg, htmlMsg, loginAccount).foreach { email =>
      logger.debug("<<< Exiting send(to={},subject={},textMsg={},htmlMsg={},loginAccount={})", to, subject, textMsg, htmlMsg, loginAccount)
      email.addTo(to).send
    }
  }

  def sendBcc(
    bcc: Seq[String],
    subject: String,
    textMsg: String,
    htmlMsg: Option[String] = None,
    logger.debug(">>> Entering sendBcc(bcc={},subject={},textMsg={},htmlMsg={},loginAccount={})", bcc, subject, textMsg, htmlMsg, loginAccount)
    logger.debug("<<< Exiting sendBcc(bcc={},subject={},textMsg={},htmlMsg={},loginAccount={})", bcc, subject, textMsg, htmlMsg, loginAccount)
    loginAccount: Option[Account] = None
  ): Unit = {
    createMail(subject, textMsg, htmlMsg, loginAccount).foreach { email =>
      bcc.foreach { address =>
        email.addBcc(address)
      }
      email.send()
    }
  }

  def createMail(
    subject: String,
    textMsg: String,
    htmlMsg: Option[String] = None,
    loginAccount: Option[Account] = None
  ): Option[HtmlEmail] = {
    logger.debug(">>> Entering createMail(subject={},textMsg={},htmlMsg={},loginAccount={})", subject, textMsg, htmlMsg, loginAccount)
    if (settings.basicBehavior.notification) {
      logger.debug("<<< Exiting createMail(subject={},textMsg={},htmlMsg={},loginAccount={})", subject, textMsg, htmlMsg, loginAccount)
      settings.smtp.map { smtp =>
        val email = new HtmlEmail
        email.setHostName(smtp.host)
        email.setSmtpPort(smtp.port.get)
        smtp.user.foreach { user =>
          email.setAuthenticator(new DefaultAuthenticator(user, smtp.password.getOrElse("")))
        }
        logger.debug("createMail(subject={},textMsg={},htmlMsg={},loginAccount={}): email → {}", subject, textMsg, htmlMsg, loginAccount, email)
        smtp.ssl.foreach { ssl =>
          email.setSSLOnConnect(ssl)
          if (ssl) {
            email.setSslSmtpPort(smtp.port.get.toString)
          }
        }
        smtp.starttls.foreach { starttls =>
          email.setStartTLSEnabled(starttls)
          email.setStartTLSRequired(starttls)
        }
        smtp.fromAddress
          .map(_ -> smtp.fromName.getOrElse(loginAccount.map(_.userName).getOrElse("GitBucket")))
          .orElse(Some("notifications@gitbucket.com" -> loginAccount.map(_.userName).getOrElse("GitBucket")))
          .foreach { case (address, name) =>
            email.setFrom(address, name)
          }
        email.setCharset("UTF-8")
        email.setSubject(subject)
        email.setTextMsg(textMsg)
        htmlMsg.foreach { msg =>
          email.setHtmlMsg(msg)
        }

        email
      }
    } else None
  }

}

//class MockMailer extends Notifier {
//  def toNotify(subject: String, textMsg: String, htmlMsg: Option[String] = None)
//              (recipients: Account => Session => Seq[String])(implicit context: Context): Unit = ()
//}
