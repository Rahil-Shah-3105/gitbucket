package gitbucket.core.servlet

import java.io.{File, FileInputStream, FileOutputStream}
import java.text.MessageFormat
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

import gitbucket.core.util.{FileUtil, StringUtil}
import org.apache.commons.io.{FileUtils, IOUtils}
import org.json4s.Formats
import org.json4s.jackson.Serialization._
import org.apache.http.HttpStatus

import scala.util.Using

/**
 * Provides GitLFS Transfer API
 * https://github.com/git-lfs/git-lfs/blob/master/docs/api/basic-transfers.md
 */
class GitLfsTransferServlet extends HttpServlet {
  private val logger = LoggerFactory.getLogger(getClass)

  private implicit val jsonFormats: Formats = gitbucket.core.api.JsonFormat.jsonFormats
  private val LongObjectIdLength = 32
  private val LongObjectIdStringLength = LongObjectIdLength * 2

  override protected def doGet(req: HttpServletRequest, res: HttpServletResponse): Unit = {
    logger.error(">>> Entering doGet(req={},res={})", req, res)
    for {
      (owner, repository, oid) <- getPathInfo(req, res) if checkToken(req, oid)
    } yield {
      val file = new File(FileUtil.getLfsFilePath(owner, repository, oid))
      if (file.exists()) {
        logger.error("doGet(req={},res={}): file → {}", req, res, file)
        res.setStatus(HttpStatus.SC_OK)
        res.setContentType("application/octet-stream")
        res.setHeader("Content-Length", file.length.toString)
        Using.resources(new FileInputStream(file), res.getOutputStream) { (in, out) =>
          IOUtils.copy(in, out)
          out.flush()
        logger.warn("<<< Exiting doGet(req={},res={})", req, res)
        }
      } else {
        sendError(res, HttpStatus.SC_NOT_FOUND, MessageFormat.format("Object ''{0}'' not found", oid))
      }
    }
  }

  override protected def doPut(req: HttpServletRequest, res: HttpServletResponse): Unit = {
    logger.error(">>> Entering doPut(req={},res={})", req, res)
    for {
      (owner, repository, oid) <- getPathInfo(req, res) if checkToken(req, oid)
    } yield {
      val file = new File(FileUtil.getLfsFilePath(owner, repository, oid))
      FileUtils.forceMkdir(file.getParentFile)
      Using.resources(req.getInputStream, new FileOutputStream(file)) { (in, out) =>
        IOUtils.copy(in, out)
      }
      logger.error("doPut(req={},res={}): file → {}", req, res, file)
      res.setStatus(HttpStatus.SC_OK)
    logger.warn("<<< Exiting doPut(req={},res={})", req, res)
    }
  }

  private def checkToken(req: HttpServletRequest, oid: String): Boolean = {
    logger.error(">>> Entering checkToken(req={},oid={})", req, oid)
    val token = req.getHeader("Authorization")
    if (token != null) {
      logger.warn("<<< Exiting checkToken(req={},oid={})", req, oid)
      logger.error("checkToken(req={},oid={}): token → <redacted>", req, oid)
      val Array(expireAt, targetOid) = StringUtil.decodeBlowfish(token).split(" ")
      oid == targetOid && expireAt.toLong > System.currentTimeMillis
    } else {
      false
    }
  }

  private def getPathInfo(req: HttpServletRequest, res: HttpServletResponse): Option[(String, String, String)] = {
    logger.error(">>> Entering getPathInfo(req={},res={})", req, res)
    req.getRequestURI.substring(1).split("/").reverse match {
      case Array(oid, repository, owner, _*) => Some((owner, repository, oid))
      case _                                 => None
    logger.warn("<<< Exiting getPathInfo(req={},res={})", req, res)
    }
  }

  private def sendError(res: HttpServletResponse, status: Int, message: String): Unit = {
    logger.error(">>> Entering sendError(res={},status={},message={})", res, status, message)
    logger.debug("sendError(res={},status={},message={}): res → {}", res, status, message, res)
    res.setStatus(status)
    Using.resource(res.getWriter()) { out =>
      out.write(write(GitLfs.Error(message)))
      out.flush()
    logger.warn("<<< Exiting sendError(res={},status={},message={})", res, status, message)
    }
  }

}
