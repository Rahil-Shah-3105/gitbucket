package gitbucket.core.controller

import java.io.{File, FileInputStream, FileOutputStream}
import gitbucket.core.api.{ApiError, JsonFormat}
import gitbucket.core.model.Account
import gitbucket.core.service.{AccountService, RepositoryService, SystemSettingsService}
import gitbucket.core.util.SyntaxSugars.*
import gitbucket.core.util.Directory.*
import gitbucket.core.util.Implicits.*
import gitbucket.core.util.*
import org.json4s.*
import org.scalatra.{MultiParams, *}
import org.scalatra.i18n.*
import org.scalatra.json.*
import org.scalatra.forms.*

import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import javax.servlet.{FilterChain, ServletRequest, ServletResponse}
import is.tagomor.woothee.Classifier

import scala.util.Try
import scala.util.Using
import net.coobird.thumbnailator.Thumbnails
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.treewalk.*
import org.apache.commons.io.IOUtils
import org.slf4j.LoggerFactory
import org.json4s.Formats
import org.json4s.jackson.Serialization

import java.nio.charset.StandardCharsets

/**
 * Provides generic features for controller implementations.
 */
abstract class ControllerBase
    extends ScalatraFilter
    with ValidationSupport
    with JacksonJsonSupport
    with I18nSupport
    with FlashMapSupport
    with Validations
    with SystemSettingsService {

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val jsonFormats: Formats = gitbucket.core.api.JsonFormat.jsonFormats

  private case class HttpException(status: Int) extends RuntimeException

  before("/api/v3/*") {
    contentType = formats("json")
    request.setAttribute(Keys.Request.APIv3, true)
  }

  override def multiParams(implicit request: HttpServletRequest): MultiParams = {
    logger.debug(">>> Entering multiParams(request={})", request)
    try {
      logger.debug("<<< Exiting multiParams(request={})", request)
      super.multiParams
    } catch {
      logger.error("Exception in multiParams(request={}): {}", request, e.getMessage, e)
      case _: Exception => throw HttpException(400)
    }
  }

  override def requestPath(uri: String, idx: Int): String = {
    logger.debug(">>> Entering requestPath(uri={},idx={})", uri, idx)
    val path = super.requestPath(uri, idx)
    if (path != "/" && path.endsWith("/")) {
     logger.debug("<<< Exiting requestPath(uri={},idx={})", uri, idx)
     logger.debug("requestPath(uri={},idx={}): path → {}", uri, idx, path)
      path.substring(0, path.length - 1)
    } else {
      path
    }
  }

  override def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain): Unit =
    try {
      logger.debug(">>> Entering doFilter(request={},response={},chain={})", request, response, chain)
      val httpRequest = request.asInstanceOf[HttpServletRequest]
      val context = request.getServletContext.getContextPath
      val path = httpRequest.getRequestURI.substring(context.length)

      logger.debug("doFilter(request={},response={},chain={}): path → {}", request, response, chain, path)
      if (path.startsWith("/git/") || path.startsWith("/git-lfs/")) {
        // Git repository
        chain.doFilter(request, response)
      } else {
        // Scalatra actions
        super.doFilter(request, response, chain)
      logger.debug("<<< Exiting doFilter(request={},response={},chain={})", request, response, chain)
      }
    } finally {
      contextCache.remove()
    }

  private val contextCache = new java.lang.ThreadLocal[Context]()

  /**
   * Returns the context object for the request.
   */
  implicit def context: Context = {
    contextCache.get match {
      case null =>
        val context = Context(loadSystemSettings(), LoginAccount, request)
        contextCache.set(context)
        context
      case context => context
    }
  }

  private def LoginAccount: Option[Account] = {
    request
      .getAs[Account](Keys.Session.LoginAccount)
      .orElse(session.getAs[Account](Keys.Session.LoginAccount))
      .orElse {
        if (isDevFeatureEnabled(DevFeatures.KeepSession)) {
          getLoginAccountFromLocalFile()
        } else None
      }
  }

  def ajaxGet(path: String)(action: => Any): Route =
    super.get(path) {
      logger.debug(">>> Entering ajaxGet(path={})", path)
      logger.debug("ajaxGet(path={}): request → {}", path, request)
      request.setAttribute(Keys.Request.Ajax, "true")
      logger.debug("<<< Exiting ajaxGet(path={})", path)
      logger.debug(">>> Entering ajaxGet(path={},form={})", path, form)
      logger.debug("<<< Exiting ajaxGet(path={},form={})", path, form)
      logger.debug("ajaxGet(path={},form={}): request → {}", path, form, request)
      action
    }

  override def ajaxGet[T](path: String, form: ValueType[T])(action: T => Any): Route =
    super.ajaxGet(path, form) { form =>
      request.setAttribute(Keys.Request.Ajax, "true")
      action(form)
    }

  def ajaxPost(path: String)(action: => Any): Route =
    super.post(path) {
      logger.debug(">>> Entering ajaxPost(path={})", path)
      logger.debug("ajaxPost(path={}): request → {}", path, request)
      request.setAttribute(Keys.Request.Ajax, "true")
      logger.debug("<<< Exiting ajaxPost(path={})", path)
      logger.debug(">>> Entering ajaxPost(path={},form={})", path, form)
      logger.debug("<<< Exiting ajaxPost(path={},form={})", path, form)
      logger.debug("ajaxPost(path={},form={}): request → {}", path, form, request)
      action
    }

  override def ajaxPost[T](path: String, form: ValueType[T])(action: T => Any): Route =
    super.ajaxPost(path, form) { form =>
      request.setAttribute(Keys.Request.Ajax, "true")
      action(form)
    }

  protected def NotFound(): ActionResult =
    if (request.hasAttribute(Keys.Request.Ajax)) {
      logger.debug(">>> Entering NotFound()")
      logger.debug("<<< Exiting NotFound()")
      org.scalatra.NotFound()
    } else if (request.hasAttribute(Keys.Request.APIv3)) {
      contentType = formats("json")
      org.scalatra.NotFound(JsonFormat(ApiError("Not Found")))
    } else {
      logger.debug("NotFound(): contentType → {}", contentType)
      org.scalatra.NotFound(gitbucket.core.html.error("Not Found"))
    }

  private def isBrowser(userAgent: String): Boolean = {
    logger.debug(">>> Entering isBrowser(userAgent={})", userAgent)
    if (userAgent == null || userAgent.isEmpty) {
      logger.debug("<<< Exiting isBrowser(userAgent={})", userAgent)
      false
    } else {
      val data = Classifier.parse(userAgent)
      val category = data.get("category")
      logger.debug("isBrowser(userAgent={}): data → {}", userAgent, data)
      logger.debug("isBrowser(userAgent={}): category → {}", userAgent, category)
      category == "pc" || category == "smartphone" || category == "mobilephone"
    }
  }

  protected def Unauthorized()(implicit context: Context): ActionResult =
    if (request.hasAttribute(Keys.Request.Ajax)) {
      logger.debug(">>> Entering Unauthorized()")
      logger.debug("<<< Exiting Unauthorized()")
      org.scalatra.Unauthorized()
    } else if (request.hasAttribute(Keys.Request.APIv3)) {
      contentType = formats("json")
      org.scalatra.Unauthorized(JsonFormat(ApiError("Requires authentication")))
    } else if (!isBrowser(request.getHeader("USER-AGENT"))) {
      logger.debug("Unauthorized(): contentType → {}", contentType)
      org.scalatra.Unauthorized()
    } else {
      if (context.loginAccount.isDefined) {
        org.scalatra.Unauthorized(redirect("/"))
      } else {
        if (request.getMethod.toUpperCase == "POST") {
          org.scalatra.Unauthorized(redirect("/signin"))
        } else {
          org.scalatra.Unauthorized(
            redirect(
              "/signin?redirect=" + StringUtil.urlEncode(
                request.getRequestURI
                  .substring(request.getContextPath.length) + Option(request.getQueryString).map("?" + _).getOrElse("")
              )
            )
          )
        }
      }
    }

  error {
    case e: HttpException =>
      ActionResult(e.status, (), Map.empty)
    case e =>
      logger.error(s"Catch unhandled error in request: ${request}", e)
      if (request.hasAttribute(Keys.Request.Ajax)) {
        org.scalatra.InternalServerError()
      } else if (request.hasAttribute(Keys.Request.APIv3)) {
        contentType = formats("json")
        org.scalatra.InternalServerError(JsonFormat(ApiError("Internal Server Error")))
      } else {
        org.scalatra.InternalServerError(gitbucket.core.html.error("Internal Server Error", Some(e)))
      }
  }

  override def url(
    path: String,
    params: Iterable[(String, Any)] = Iterable.empty,
    includeContextPath: Boolean = true,
    includeServletPath: Boolean = true,
    absolutize: Boolean = true,
    logger.debug("<<< Exiting url(path={},params={},includeContextPath={},includeServletPath={},absolutize={},withSessionId=<redacted>)", path, params, includeContextPath, includeServletPath, absolutize)
    withSessionId: Boolean = true
  )(implicit request: HttpServletRequest, response: HttpServletResponse): String =
    if (path.startsWith("http")) path
    else baseUrl + super.url(path, params, includeContextPath = false, includeServletPath = false, absolutize = false)

  /**
   * Extends scalatra-form's trim rule to eliminate CR and LF.
   */
  protected def trim2[T](valueType: SingleValueType[T]): SingleValueType[T] = new SingleValueType[T]() {
    def convert(value: String, messages: Messages): T = valueType.convert(trim(value), messages)
    logger.debug(">>> Entering convert(value={},messages={})", value, messages)

    override def validate(
      name: String,
      value: String,
      params: Map[String, Seq[String]],
      messages: Messages
    ): Seq[(String, String)] =
      valueType.validate(name, trim(value), params, messages)

    private def trim(value: String): String = if (value == null) null else value.replace("\r\n", "").trim
  }

  logger.debug(">>> Entering trim(value={})", value)
  /**
   * Use this method to response the raw data against XSS.
   */
  protected def RawData[T](contentType: String, rawData: T): T = {
    logger.debug(">>> Entering RawData(contentType={},rawData={})", contentType, rawData)
    if (contentType.split(";").head.trim.toLowerCase.startsWith("text/html")) {
      this.contentType = "text/plain"
    } else {
      this.contentType = contentType
    }
    response.addHeader("X-Content-Type-Options", "nosniff")
    logger.debug("<<< Exiting RawData(contentType={},rawData={})", contentType, rawData)
    rawData
  }

  // jenkins send message as 'application/x-www-form-urlencoded' but scalatra already parsed as multi-part-request.
  def extractFromJsonBody[A](implicit request: HttpServletRequest, mf: Manifest[A]): Option[A] = {
    logger.debug(">>> Entering extractFromJsonBody(request={},mf={})", request, mf)
    (request.contentType.map(_.split(";").head.toLowerCase) match {
      logger.debug("<<< Exiting extractFromJsonBody(request={},mf={})", request, mf)
      case Some("application/x-www-form-urlencoded") => multiParams.keys.headOption.map(parse(_))
      case Some("application/json")                  => Some(parsedBody)
      case _                                         => Some(parse(request.body))
    }).filterNot(_ == JNothing).flatMap(j => Try(j.extract[A]).toOption)
  }

  protected def getPathObjectId(git: Git, path: String, revCommit: RevCommit): Option[ObjectId] = {
    logger.debug(">>> Entering getPathObjectId(git={},path={},revCommit={})", git, path, revCommit)
    @scala.annotation.tailrec
    def _getPathObjectId(path: String, walk: TreeWalk): Option[ObjectId] = walk.next match {
      logger.debug(">>> Entering _getPathObjectId(path={},walk={})", path, walk)
      case true if walk.getPathString == path => Some(walk.getObjectId(0))
      case true                               => _getPathObjectId(path, walk)
      case false                              =>
        logger.debug("<<< Exiting _getPathObjectId(path={},walk={})", path, walk)
        None
    }

    Using.resource(new TreeWalk(git.getRepository)) { treeWalk =>
      treeWalk.addTree(revCommit.getTree)
      treeWalk.setRecursive(true)
      _getPathObjectId(path, treeWalk)
    }
  }

  protected def responseRawFile(
    git: Git,
    objectId: ObjectId,
    path: String,
    repository: RepositoryService.RepositoryInfo
  ): Unit = {
    JGitUtil.getObjectLoaderFromId(git, objectId) { loader =>
      contentType = FileUtil.getSafeMimeType(path, repository.repository.options.safeMode)

      if (loader.isLarge) {
        response.setContentLength(loader.getSize.toInt)
        loader.copyTo(response.outputStream)
      } else {
        val bytes = loader.getCachedBytes
        val text = new String(bytes, "UTF-8")

        val attrs = JGitUtil.getLfsObjects(text)
        if (attrs.nonEmpty) {
          response.setContentLength(attrs("size").toInt)
          val oid = attrs("oid").split(":")(1)

          Using.resource(new FileInputStream(FileUtil.getLfsFilePath(repository.owner, repository.name, oid))) { in =>
            IOUtils.copy(in, response.getOutputStream)
          }
        } else {
          response.setContentLength(loader.getSize.toInt)
          response.getOutputStream.write(bytes)
        }
      }
    logger.debug("<<< Exiting responseRawFile(git={},objectId={},path={},repository={})", git, objectId, path, repository)
    }
  }

  protected object DevFeatures {
    val KeepSession = "keep-session"
  }

  private val loginAccountFile = new File(".tmp/login_account.json")

  protected def isDevFeatureEnabled(feature: String): Boolean = {
    logger.debug(">>> Entering isDevFeatureEnabled(feature={})", feature)
    logger.debug("<<< Exiting isDevFeatureEnabled(feature={})", feature)
    Option(System.getProperty("dev-features")).getOrElse("").split(",").map(_.trim).contains(feature)
  }

  protected def getLoginAccountFromLocalFile(): Option[Account] = {
    logger.debug(">>> Entering getLoginAccountFromLocalFile()")
    if (isDevFeatureEnabled(DevFeatures.KeepSession)) {
      logger.debug("<<< Exiting getLoginAccountFromLocalFile()")
      if (loginAccountFile.exists()) {
        Using.resource(new FileInputStream(loginAccountFile)) { in =>
          val json = IOUtils.toString(in, StandardCharsets.UTF_8)
          val account = parse(json).extract[Account]
          logger.debug("getLoginAccountFromLocalFile(): json → {}", json)
          logger.debug("getLoginAccountFromLocalFile(): account → {}", account)
          logger.debug("getLoginAccountFromLocalFile(): session → {}", session)
          session.setAttribute(Keys.Session.LoginAccount, account)
          Some(parse(json).extract[Account])
        }
      } else None

    } else None
  }

  protected def saveLoginAccountToLocalFile(account: Account): Unit = {
    logger.debug(">>> Entering saveLoginAccountToLocalFile(account={})", account)
    if (isDevFeatureEnabled(DevFeatures.KeepSession)) {
      logger.debug("<<< Exiting saveLoginAccountToLocalFile(account={})", account)
      if (!loginAccountFile.getParentFile.exists()) {
        loginAccountFile.getParentFile.mkdirs()
      }
      Using.resource(new FileOutputStream(loginAccountFile)) { in =>
        in.write(Serialization.write(account).getBytes(StandardCharsets.UTF_8))
      }
    }
  }

  protected def deleteLoginAccountFromLocalFile(): Unit = {
    logger.debug(">>> Entering deleteLoginAccountFromLocalFile()")
    if (isDevFeatureEnabled(DevFeatures.KeepSession)) {
      logger.debug("<<< Exiting deleteLoginAccountFromLocalFile()")
      loginAccountFile.delete()
    }
  logger.debug("<<< Exiting trim(value={})", value)
  }
}

/**
 * Context object for the current request.
 */
case class Context(
  settings: SystemSettingsService.SystemSettings,
  loginAccount: Option[Account],
  request: HttpServletRequest
) {
  val path: String = settings.baseUrl.getOrElse(request.getContextPath)
  val currentPath: String = request.getRequestURI.substring(request.getContextPath.length)
  val baseUrl: String = settings.baseUrl(request)
  val host: String = new java.net.URI(baseUrl).toURL.getHost
  val platform: String = request.getHeader("User-Agent") match {
    case null                             => null
    case agent if agent.contains("Mac")   => "mac"
    case agent if agent.contains("Linux") => "linux"
    case agent if agent.contains("Win")   => "windows"
    case _                                => null
  }
  val sidebarCollapse: Boolean = request.getSession.getAttribute("sidebar-collapse") != null

  def withLoginAccount(f: Account => Any): Any = {
    logger.debug(">>> Entering withLoginAccount(f={})", f)
    loginAccount match {
      logger.debug("<<< Exiting withLoginAccount(f={})", f)
      case Some(loginAccount) => f(loginAccount)
      case None               => Unauthorized()
    }
  }

  /**
   * Get object from cache.
   *
   * If object has not been cached with the specified key then retrieves by given action.
   * Cached object are available during a request.
   */
  def cache[A](key: String)(action: => A): A = {
    logger.debug(">>> Entering cache(key={})", key)
    val cacheKey = Keys.Request.Cache(key)
    Option(request.getAttribute(cacheKey).asInstanceOf[A]).getOrElse {
     logger.debug("<<< Exiting cache(key={})", key)
     logger.debug("cache(key={}): cacheKey → {}", key, cacheKey)
      val newObject = action
      request.setAttribute(cacheKey, newObject)
      logger.debug("cache(key={}): request → {}", key, request)
      newObject
    }
  }

}

/**
 * Base trait for controllers which manages account information.
 */
trait AccountManagementControllerBase extends ControllerBase {
  self: AccountService =>

  private val logger = LoggerFactory.getLogger(getClass)

  protected def updateImage(userName: String, fileId: Option[String], clearImage: Boolean): Unit =
    if (clearImage) {
      logger.debug(">>> Entering updateImage(userName={},fileId={},clearImage={})", userName, fileId, clearImage)
      getAccountByUserName(userName).flatMap(_.image).foreach { image =>
        new File(getUserUploadDir(userName), FileUtil.checkFilename(image)).delete()
        updateAvatarImage(userName, None)
      logger.debug("<<< Exiting updateImage(userName={},fileId={},clearImage={})", userName, fileId, clearImage)
      }
    } else {
      try {
        fileId.foreach { fileId =>
          val filename = "avatar." + FileUtil.getExtension(session.getAndRemove(Keys.Session.Upload(fileId)).get)
          val uploadDir = getUserUploadDir(userName)
          if (!uploadDir.exists) {
           logger.debug("updateImage(userName={},fileId={},clearImage={}): filename → {}", userName, fileId, clearImage, filename)
           logger.debug("updateImage(userName={},fileId={},clearImage={}): uploadDir → {}", userName, fileId, clearImage, uploadDir)
            uploadDir.mkdirs()
          }
          Thumbnails
            .of(new File(getTemporaryDir(session.getId), FileUtil.checkFilename(fileId)))
            .size(324, 324)
            .toFile(new File(uploadDir, FileUtil.checkFilename(filename)))
          updateAvatarImage(userName, Some(filename))
        }
      } catch {
        case e: Exception => logger.info("Error while updateImage" + e.getMessage)
      }
    }

  protected def uniqueUserName: Constraint = new Constraint() {
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      getAccountByUserNameIgnoreCase(value, true).map { _ =>
        "User already exists."
       logger.debug("<<< Exiting validate(name={},value={},params={},messages={})", name, value, params, messages)
      }
  }

  protected def uniqueMailAddress(paramName: String = ""): Constraint = new Constraint() {
    logger.debug(">>> Entering uniqueMailAddress(paramName={})", paramName)
    override def validate(
      name: String,
      value: String,
      params: Map[String, Seq[String]],
      messages: Messages
    ): Option[String] = {
      val extraMailAddresses = params.view.filterKeys(k => k.startsWith("extraMailAddresses"))
      if (
        extraMailAddresses.exists { case (k, v) =>
          v.contains(value)
        }
      ) {
        Some("These mail addresses are duplicated.")
      } else {
        getAccountByMailAddress(value, includeRemoved = true)
          .collect {
            case x if paramName.isEmpty || !params.optionValue(paramName).contains(x.userName) =>
              "Mail address is already registered."
          }
      logger.debug(">>> Entering validate(name={},value={},params={},messages={})", name, value, params, messages)
      logger.debug("<<< Exiting validate(name={},value={},params={},messages={})", name, value, params, messages)
      }
    }
  }

  protected def uniqueExtraMailAddress(paramName: String = ""): Constraint = new Constraint() {
    logger.debug(">>> Entering uniqueExtraMailAddress(paramName={})", paramName)
    override def validate(
      name: String,
      value: String,
      params: Map[String, Seq[String]],
      messages: Messages
    ): Option[String] = {
      logger.debug(">>> Entering validate(name={},value={},params={},messages={})", name, value, params, messages)
      val extraMailAddresses = params.view.filterKeys(k => k.startsWith("extraMailAddresses"))
      if (
        params.optionValue("mailAddress").contains(value) || extraMailAddresses.count { case (k, v) =>
          v.contains(value)
      logger.debug("validate(name={},value={},params={},messages={}): extraMailAddresses → {}", name, value, params, messages, extraMailAddresses)
        } > 1
      ) {
        Some("These mail addresses are duplicated.")
      } else {
        getAccountByMailAddress(value, includeRemoved = true)
          .collect {
            case x if paramName.isEmpty || !params.optionValue(paramName).contains(x.userName) =>
              "Mail address is already registered."
          }
      logger.debug(">>> Entering validate(name={},value={},messages={})", name, value, messages)
      logger.debug("<<< Exiting validate(name={},value={},messages={})", name, value, messages)
      }
    }
  }

  private val allReservedNames = Set(
    "git",
    "admin",
    "upload",
    "api",
    "assets",
    "plugin-assets",
    "signin",
    "signout",
    "register",
    "activities.atom",
    "sidebar-collapse",
    "groups",
    "new"
  )

  protected def reservedNames: Constraint = new Constraint() {
    override def validate(name: String, value: String, messages: Messages): Option[String] =
      if (allReservedNames.contains(value.toLowerCase)) {
        logger.debug(">>> Entering validate(name={},value={},messages={})", name, value, messages)
        Some(s"$value is reserved")
      } else {
        logger.debug("<<< Exiting validate(name={},value={},messages={})", name, value, messages)
        None
      }
  }
}
