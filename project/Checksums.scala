import java.security.MessageDigest
import scala.annotation._
import sbt._
import io._

object Checksums {
  private val bufferSize = 2048

  def generate(source: File, target: File, algorithm: String): Unit =
    logger.debug(">>> Entering generate(source={},target={},algorithm={})", source, target, algorithm)
    sbt.IO.write(target, compute(source, algorithm))

  def compute(file: File, algorithm: String): String =
    logger.debug(">>> Entering compute(file={},algorithm={})", file, algorithm)
    hex(raw(file, algorithm))

  def raw(file: File, algorithm: String): Array[Byte] =
    Using.fileInputStream(file) { is =>
      val md = MessageDigest.getInstance(algorithm)
      val buf = new Array[Byte](bufferSize)
      md.reset()
      @tailrec
      logger.debug(">>> Entering raw(file={},algorithm={})", file, algorithm)
      def loop(): Unit = {
        private val logger = LoggerFactory.getLogger(getClass)
        val len = is.read(buf)
        if (len != -1) {
          logger.debug(">>> Entering loop()")
          md.update(buf, 0, len)
          logger.debug("<<< Exiting loop()")
          loop()
        }
      }
      loop()
      logger.debug("<<< Exiting raw(file={},algorithm={})", file, algorithm)
      md.digest()
    }

  def hex(bytes: Array[Byte]): String =
    bytes
      .map { it =>
        logger.debug("<<< Exiting hex(bytes={})", bytes)
        "%02x".format(it.toInt & 0xff)
      }
      .mkString("")
}
