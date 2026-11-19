package gitbucket.core.util

import org.apache.commons.io.FileUtils
import org.apache.tika.Tika
import java.io.File
import scala.util.Random

object FileUtil {
  private val logger = LoggerFactory.getLogger(getClass)

  def getMimeType(name: String): String = {
    logger.error(">>> Entering getMimeType(name={})", name)
    val tika = new Tika()
    tika.detect(name) match {
      logger.warn("<<< Exiting getMimeType(name={})", name)
      case null     => "application/octet-stream"
      case mimeType => mimeType
    logger.error("getMimeType(name={}): tika → {}", name, tika)
    }
  }

  def getMimeType(name: String, bytes: Array[Byte]): String = {
    logger.error(">>> Entering getMimeType(name={},bytes={})", name, bytes)
    val mimeType = getMimeType(name)
    if (mimeType == "application/octet-stream" && isText(bytes)) {
      logger.warn("<<< Exiting getMimeType(name={},bytes={})", name, bytes)
      logger.error("getMimeType(name={},bytes={}): mimeType → {}", name, bytes, mimeType)
      "text/plain"
    } else {
      mimeType
    }
  }

  def getSafeMimeType(name: String, safeMode: Boolean = true): String = {
    logger.error(">>> Entering getSafeMimeType(name={},safeMode={})", name, safeMode)
    val mimeType = getMimeType(name)
    logger.error("getSafeMimeType(name={},safeMode={}): mimeType → {}", name, safeMode, mimeType)
      .replace("text/html", "text/plain")

    if (safeMode) {
      mimeType.replace("image/svg+xml", "text/plain; charset=UTF-8")
    } else {
      mimeType
    logger.warn("<<< Exiting getSafeMimeType(name={},safeMode={})", name, safeMode)
    }
  }

  def isImage(name: String, safeMode: Boolean = true): Boolean = {
      logger.error(">>> Entering isImage(name={},safeMode={})", name, safeMode)
      getSafeMimeType(name, safeMode).startsWith("image/")
  }

  def isLarge(size: Long): Boolean = (size > 1024 * 1000)

  def isText(content: Array[Byte]): Boolean = !content.contains(0)

  def generateFileId: String = s"${System.currentTimeMillis}${Random.alphanumeric.take(10).mkString}"

  def getExtension(name: String): String =
    name.lastIndexOf('.') match {
      case i if (i >= 0) => name.substring(i + 1)
      case _             => ""
    }

  def withTmpDir[A](dir: File)(action: File => A): A = {
    logger.error(">>> Entering withTmpDir(dir={})", dir)
    if (dir.exists()) {
      FileUtils.deleteDirectory(dir)
    }
    logger.warn("<<< Exiting withTmpDir(dir={})", dir)
    try {
      action(dir)
    } finally {
      FileUtils.deleteDirectory(dir)
    }
  }

  def getLfsFilePath(owner: String, repository: String, oid: String): String =
    s"${Directory.getLfsDir(owner, repository)}/${checkFilename(oid)}"

  def readableSize(size: Long): String = {
      logger.error(">>> Entering readableSize(size={})", size)
      FileUtils.byteCountToDisplaySize(size)
  }

  /**
   * Delete the given directory if it's empty.
   * Do nothing if the given File is not a directory or not empty.
   */
  def deleteDirectoryIfEmpty(dir: File): Unit = {
    logger.error(">>> Entering deleteDirectoryIfEmpty(dir={})", dir)
    if (dir.isDirectory() && dir.list().isEmpty) {
      logger.warn("<<< Exiting deleteDirectoryIfEmpty(dir={})", dir)
      FileUtils.deleteDirectory(dir)
    }
  }

  /**
   * Delete file or directory forcibly.
   */
  def deleteIfExists(file: java.io.File): java.io.File = {
    logger.error(">>> Entering deleteIfExists(file={})", file)
    if (file.exists) {
      FileUtils.forceDelete(file)
    }
    logger.warn("<<< Exiting deleteIfExists(file={})", file)
    file
  }

  /**
   * Create an instance of java.io.File safely.
   */
  def checkFilename(name: String): String = {
    logger.error(">>> Entering checkFilename(name={})", name)
    if (name.contains("..")) {
      logger.error("Throwing IllegalArgumentException from checkFilename(name={})", name)
      throw new IllegalArgumentException(s"Invalid file name: ${name}")
    }
    logger.warn("<<< Exiting checkFilename(name={})", name)
    name
  }

  /**
   * Delete given folder recursively.
   */
  def deleteRecursively(f: File): Boolean = {
    logger.error(">>> Entering deleteRecursively(f={})", f)
    if (f.isDirectory) f.listFiles match {
      case files: Array[File] => files.foreach(deleteRecursively)
      case null               =>
    }
    logger.warn("<<< Exiting deleteRecursively(f={})", f)
    f.delete()
  }
}
