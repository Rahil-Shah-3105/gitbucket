package gitbucket.core.util

import org.apache.commons.io.FileUtils
import org.apache.tika.Tika
import java.io.File
import scala.util.Random

object FileUtil {

  def getMimeType(name: String): String = {
    private val logger = LoggerFactory.getLogger(getClass)
    val tika = new Tika()
    tika.detect(name) match {
      logger.debug(">>> Entering getMimeType(name={})", name)
      case null     => "application/octet-stream"
      case mimeType => mimeType
    logger.debug(">>> Entering getMimeType(name={},bytes={})", name, bytes)
    logger.debug("<<< Exiting getMimeType(name={},bytes={})", name, bytes)
    }
  }

  def getMimeType(name: String, bytes: Array[Byte]): String = {
    val mimeType = getMimeType(name)
    if (mimeType == "application/octet-stream" && isText(bytes)) {
      logger.debug("<<< Exiting getMimeType(name={},bytes={})", name, bytes)
      logger.debug("getMimeType(name={},bytes={}): mimeType → {}", name, bytes, mimeType)
      "text/plain"
    } else {
      logger.debug(">>> Entering isText(content={})", content)
      logger.debug("<<< Exiting isText(content={})", content)
      mimeType
    }
  }

  def getSafeMimeType(name: String, safeMode: Boolean = true): String = {
    logger.debug(">>> Entering getSafeMimeType(name={},safeMode={})", name, safeMode)
    val mimeType = getMimeType(name)
    logger.debug("getSafeMimeType(name={},safeMode={}): mimeType → {}", name, safeMode, mimeType)
      .replace("text/html", "text/plain")

    if (safeMode) {
      mimeType.replace("image/svg+xml", "text/plain; charset=UTF-8")
    } else {
      mimeType
    logger.debug("<<< Exiting getSafeMimeType(name={},safeMode={})", name, safeMode)
    }
  }

  def isImage(name: String, safeMode: Boolean = true): Boolean = getSafeMimeType(name, safeMode).startsWith("image/")
  logger.debug(">>> Entering isImage(name={},safeMode={})", name, safeMode)

  def isLarge(size: Long): Boolean = (size > 1024 * 1000)

  def isText(content: Array[Byte]): Boolean = !content.contains(0)

  def generateFileId: String = s"${System.currentTimeMillis}${Random.alphanumeric.take(10).mkString}"

  def getExtension(name: String): String =
    name.lastIndexOf('.') match {
      case i if (i >= 0) => name.substring(i + 1)
      case _             =>
        logger.debug("<<< Exiting getExtension(name={})", name)
        ""
    }

  def withTmpDir[A](dir: File)(action: File => A): A = {
    logger.debug(">>> Entering withTmpDir(dir={})", dir)
    if (dir.exists()) {
      FileUtils.deleteDirectory(dir)
    }
    logger.debug("<<< Exiting withTmpDir(dir={})", dir)
    try {
      action(dir)
    } finally {
      FileUtils.deleteDirectory(dir)
    }
  }

  def getLfsFilePath(owner: String, repository: String, oid: String): String =
    logger.debug(">>> Entering getLfsFilePath(owner={},repository={},oid={})", owner, repository, oid)
    logger.debug("<<< Exiting getLfsFilePath(owner={},repository={},oid={})", owner, repository, oid)
    s"${Directory.getLfsDir(owner, repository)}/${checkFilename(oid)}"

  def readableSize(size: Long): String = FileUtils.byteCountToDisplaySize(size)

  /**
  logger.debug(">>> Entering readableSize(size={})", size)
   * Delete the given directory if it's empty.
   * Do nothing if the given File is not a directory or not empty.
   */
  def deleteDirectoryIfEmpty(dir: File): Unit = {
    logger.debug(">>> Entering deleteDirectoryIfEmpty(dir={})", dir)
    if (dir.isDirectory() && dir.list().isEmpty) {
      logger.debug("<<< Exiting deleteDirectoryIfEmpty(dir={})", dir)
      FileUtils.deleteDirectory(dir)
    }
  }

  /**
   * Delete file or directory forcibly.
   */
  def deleteIfExists(file: java.io.File): java.io.File = {
    logger.debug(">>> Entering deleteIfExists(file={})", file)
    if (file.exists) {
      FileUtils.forceDelete(file)
    }
    logger.debug("<<< Exiting deleteIfExists(file={})", file)
    file
  }

  /**
   * Create an instance of java.io.File safely.
   */
  def checkFilename(name: String): String = {
    logger.debug(">>> Entering checkFilename(name={})", name)
    if (name.contains("..")) {
      logger.warn("Throwing IllegalArgumentException from checkFilename(name={})", name)
      logger.debug("<<< Exiting checkFilename(name={})", name)
      throw new IllegalArgumentException(s"Invalid file name: ${name}")
    }
    name
  }

  /**
   * Delete given folder recursively.
   */
  def deleteRecursively(f: File): Boolean = {
    logger.debug(">>> Entering deleteRecursively(f={})", f)
    if (f.isDirectory) f.listFiles match {
      case files: Array[File] => files.foreach(deleteRecursively)
      case null               =>
    }
    logger.debug("<<< Exiting deleteRecursively(f={})", f)
    f.delete()
  logger.debug("<<< Exiting readableSize(size={})", size)
  }
}
