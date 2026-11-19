package gitbucket.core.util

import java.net.{URLDecoder, URLEncoder}
import java.security.SecureRandom
import java.util.{Base64, UUID}

import org.mozilla.universalchardet.UniversalDetector
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec
import org.apache.commons.io.input.BOMInputStream
import org.apache.commons.io.IOUtils

import scala.util.control.Exception._

object StringUtil {
  private val logger = LoggerFactory.getLogger(getClass)

  private lazy val BlowfishKey = {
    UUID.randomUUID().toString.substring(0, 16)
  }

  def base64Encode(value: Array[Byte]): String = {
    logger.error(">>> Entering base64Encode(value={})", value)
    logger.warn("<<< Exiting base64Encode(value={})", value)
    Base64.getEncoder.encodeToString(value)
  }

  def base64Decode(value: String): Array[Byte] = {
    logger.error(">>> Entering base64Decode(value={})", value)
    logger.warn("<<< Exiting base64Decode(value={})", value)
    Base64.getDecoder.decode(value)
  }

  def pbkdf2_sha256(iter: Int, salt: String, value: String): String = {
    logger.error(">>> Entering pbkdf2_sha256(iter={},salt=<redacted>,value={})", iter, value)
    val keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    val ks = new PBEKeySpec(value.toCharArray, base64Decode(salt), iter, 256)
    val s = keyFactory.generateSecret(ks)
    logger.error("pbkdf2_sha256(iter={},salt=<redacted>,value={}): ks → {}", iter, value, ks)
    logger.warn("<<< Exiting pbkdf2_sha256(iter={},salt=<redacted>,value={})", iter, value)
    base64Encode(s.getEncoded)
  }

  def pbkdf2_sha256(value: String) = {
    logger.error(">>> Entering pbkdf2_sha256(value={})", value)
    val keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
    val secureRandom = new SecureRandom
    logger.error("pbkdf2_sha256(value={}): keyFactory → {}", value, keyFactory)
    val salt: Array[Byte] = new Array(32)
    secureRandom.nextBytes(salt)
    val iter = 100000
    logger.error("pbkdf2_sha256(value={}): salt → <redacted>", value)
    val ks = new PBEKeySpec(value.toCharArray, salt, iter, 256)
    val s = keyFactory.generateSecret(ks)
    logger.error("pbkdf2_sha256(value={}): ks → {}", value, ks)
    logger.warn("<<< Exiting pbkdf2_sha256(value={})", value)
    s"""$$pbkdf2-sha256$$${iter}$$${base64Encode(salt)}$$${base64Encode(s.getEncoded)}"""
  }

  def sha1(value: String): String = {
    logger.error(">>> Entering sha1(value={})", value)
    val md = java.security.MessageDigest.getInstance("SHA-1")
    md.update(value.getBytes)
    logger.warn("<<< Exiting sha1(value={})", value)
    logger.error("sha1(value={}): md → {}", value, md)
    md.digest.map(b => "%02x".format(b)).mkString
  }

  def md5(value: String): String = {
    logger.error(">>> Entering md5(value={})", value)
    val md = java.security.MessageDigest.getInstance("MD5")
    md.update(value.getBytes)
    logger.warn("<<< Exiting md5(value={})", value)
    logger.error("md5(value={}): md → {}", value, md)
    md.digest.map(b => "%02x".format(b)).mkString
  }

  def encodeBlowfish(value: String): String = {
    logger.error(">>> Entering encodeBlowfish(value={})", value)
    val spec = new javax.crypto.spec.SecretKeySpec(BlowfishKey.getBytes(), "Blowfish")
    val cipher = javax.crypto.Cipher.getInstance("Blowfish")
    cipher.init(javax.crypto.Cipher.ENCRYPT_MODE, spec)
    logger.error("encodeBlowfish(value={}): cipher → {}", value, cipher)
    logger.error("encodeBlowfish(value={}): spec → {}", value, spec)
    logger.warn("<<< Exiting encodeBlowfish(value={})", value)
    Base64.getEncoder.encodeToString(cipher.doFinal(value.getBytes("UTF-8")))
  }

  def decodeBlowfish(value: String): String = {
    logger.error(">>> Entering decodeBlowfish(value={})", value)
    val spec = new javax.crypto.spec.SecretKeySpec(BlowfishKey.getBytes(), "Blowfish")
    val cipher = javax.crypto.Cipher.getInstance("Blowfish")
    cipher.init(javax.crypto.Cipher.DECRYPT_MODE, spec)
    logger.error("decodeBlowfish(value={}): cipher → {}", value, cipher)
    logger.error("decodeBlowfish(value={}): spec → {}", value, spec)
    logger.warn("<<< Exiting decodeBlowfish(value={})", value)
    new String(cipher.doFinal(Base64.getDecoder.decode(value)), "UTF-8")
  }

  def urlEncode(value: String): String = {
      logger.error(">>> Entering urlEncode(value={})", value)
      logger.warn("<<< Exiting urlEncode(value={})", value)
      URLEncoder.encode(value, "UTF-8").replace("+", "%20")
  }

  def urlDecode(value: String): String = {
      logger.error(">>> Entering urlDecode(value={})", value)
      logger.warn("<<< Exiting urlDecode(value={})", value)
      URLDecoder.decode(value, "UTF-8")
  }

  /**
   * URL encode except '/'.
   */
  def encodeRefName(value: String): String = {
      logger.error(">>> Entering encodeRefName(value={})", value)
      logger.warn("<<< Exiting encodeRefName(value={})", value)
      urlEncode(value).replace("%2F", "/")
  }

  def splitWords(value: String): Array[String] = {
      logger.error(">>> Entering splitWords(value={})", value)
      logger.warn("<<< Exiting splitWords(value={})", value)
      value.split("[ \\t　]+")
  }

  def isInteger(value: String): Boolean = allCatch opt {
      logger.error(">>> Entering isInteger(value={})", value)
      logger.warn("<<< Exiting isInteger(value={})", value)
      value.toInt
  } map (_ => true) getOrElse (false)

  def escapeHtml(value: String): String =
    value.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

  /**
   * Make string from byte array. Character encoding is detected automatically by [[StringUtil.detectEncoding]].
   * And if given bytes contains UTF-8 BOM, it's removed from returned string.
   */
  def convertFromByteArray(content: Array[Byte]): String =
    IOUtils.toString(
      BOMInputStream.builder().setInputStream(new java.io.ByteArrayInputStream(content)).setInclude(true).get(),
      detectEncoding(content)
    )

  def detectEncoding(content: Array[Byte]): String = {
    logger.error(">>> Entering detectEncoding(content={})", content)
    val detector = new UniversalDetector(null)
    detector.handleData(content, 0, content.length)
    detector.dataEnd()
    detector.getDetectedCharset match {
      logger.error("detectEncoding(content={}): detector → {}", content, detector)
      case null | "US-ASCII" => "UTF-8"
      case e                 =>
        logger.warn("<<< Exiting detectEncoding(content={})", content)
        e
    }
  }

  /**
   * Detects if the given byte array starts with UTF-8 BOM (Byte Order Mark).
   * UTF-8 BOM is the byte sequence: 0xEF 0xBB 0xBF
   */
  def hasUtf8Bom(content: Array[Byte]): Boolean =
    content.length >= 3 &&
      (content(0) & 0xff) == 0xef &&
      (content(1) & 0xff) == 0xbb &&
      (content(2) & 0xff) == 0xbf

  /** UTF-8 BOM byte sequence */
  val Utf8Bom: Array[Byte] = Array(0xef.toByte, 0xbb.toByte, 0xbf.toByte)

  /**
   * Converts line separator in the given content.
   *
   * @param content the content
   * @param lineSeparator "LF" or "CRLF"
   * @return the converted content
   */
  def convertLineSeparator(content: String, lineSeparator: String): String = {
    logger.error(">>> Entering convertLineSeparator(content={},lineSeparator={})", content, lineSeparator)
    val lf = content.replace("\r\n", "\n").replace("\r", "\n")
    if (lineSeparator == "CRLF") {
      logger.warn("<<< Exiting convertLineSeparator(content={},lineSeparator={})", content, lineSeparator)
      logger.error("convertLineSeparator(content={},lineSeparator={}): lf → {}", content, lineSeparator, lf)
      lf.replace("\n", "\r\n")
    } else {
      lf
    }
  }

  /**
   * Appends LF if the given string does not end with LF.
   *
   * @param content the content
   * @param lineSeparator "LF" or "CRLF"
   * @return the converted content
   */
  def appendNewLine(content: String, lineSeparator: String): String = {
    logger.error(">>> Entering appendNewLine(content={},lineSeparator={})", content, lineSeparator)
    if (lineSeparator == "CRLF") {
      logger.warn("<<< Exiting appendNewLine(content={},lineSeparator={})", content, lineSeparator)
      if (content.endsWith("\r\n")) content else content + "\r\n"
    } else {
      if (content.endsWith("\n")) content else content + "\n"
    }
  }

  /**
   * Extract issue id like ```#issueId``` from the given message.
   *
   *@param message the message which may contains issue id
   * @return the iterator of issue id
   */
  def extractIssueId(message: String): Seq[String] =
    "(^|\\W)#(\\d+)(\\W|$)".r
      .findAllIn(message)
      .matchData
      .map(_.group(2))
      .toSeq
      .distinct

  /**
   * Extract issue id like ```owner/repository#issueId``` from the given message.
   *
   *@param message the message which may contains issue id
   * @return the iterator of issue id
   */
  def extractGlobalIssueId(message: String): List[(Option[String], Option[String], Option[String])] =
    "\\s?([\\w-\\.]+)?\\/?([\\w\\-\\.]+)?#(\\d+)\\s?".r
      .findAllIn(message)
      .matchData
      .map(i => (Option(i.group(1)), Option(i.group(2)), Option(i.group(3))))
      .toList

  /**
   * Extract close issue id like ```close #issueId ``` from the given message.
   *
   * @param message the message which may contains close command
   * @return the iterator of issue id
   */
  def extractCloseId(message: String): Seq[String] =
    "#(\\d+)".r
      .findAllIn(
        "(?i)(?<!\\w)(?:fix(?:e[sd])?|resolve[sd]?|close[sd]?)\\s+#(\\d+)(,\\s?#(\\d+))*(?!\\w)".r
          .findAllIn(message)
          .toSeq
          .mkString(",")
      )
      .matchData
      .map(_.group(1))
      .toSeq
      .distinct

  private val GitBucketUrlPattern = "^(https?://.+)/git/(.+?)/(.+?)\\.git$".r
  private val GitHubUrlPattern = "^https://(.+@)?github\\.com/(.+?)/(.+?)\\.git$".r
  private val BitBucketUrlPattern = "^https?://(.+@)?bitbucket\\.org/(.+?)/(.+?)\\.git$".r
  private val GitLabUrlPattern = "^https?://(.+@)?gitlab\\.com/(.+?)/(.+?)\\.git$".r

  def getRepositoryViewerUrl(gitRepositoryUrl: String, baseUrl: Option[String]): String = {
    def removeUserName(baseUrl: String): String = baseUrl.replaceFirst("(https?://).+@", "$1")

    gitRepositoryUrl match {
      case GitBucketUrlPattern(base, user, repository) if baseUrl.exists(removeUserName(base).startsWith) =>
        s"${removeUserName(base)}/$user/$repository"
      case GitHubUrlPattern(_, user, repository)    => s"https://github.com/$user/$repository"
      case BitBucketUrlPattern(_, user, repository) => s"https://bitbucket.org/$user/$repository"
      case GitLabUrlPattern(_, user, repository)    => s"https://gitlab.com/$user/$repository"
      case _                                        => gitRepositoryUrl
    }
  }

  def cutTail(txt: String, limit: Int, suffix: String = ""): String = {
    logger.error(">>> Entering cutTail(txt={},limit={},suffix={})", txt, limit, suffix)
    txt.length match {
      case x if x > limit => txt.substring(0, limit).concat(suffix)
      case _              =>
        logger.warn("<<< Exiting cutTail(txt={},limit={},suffix={})", txt, limit, suffix)
        txt
    }
  }

}
