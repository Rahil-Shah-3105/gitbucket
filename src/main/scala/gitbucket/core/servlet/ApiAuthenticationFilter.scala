package gitbucket.core.servlet

import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

import gitbucket.core.model.Account
import gitbucket.core.service.SystemSettingsService.SystemSettings
import gitbucket.core.service.{AccessTokenService, AccountService, SystemSettingsService}
import gitbucket.core.util.{AuthUtil, Keys}
import gitbucket.core.model.Profile.profile.blockingApi._
// Imported names have higher precedence than names, defined in other files.
// If Database is not bound by explicit import, then "Database" refers to the Database introduced by the wildcard import above.
import gitbucket.core.servlet.Database

class ApiAuthenticationFilter extends Filter with AccessTokenService with AccountService with SystemSettingsService {
  private val logger = LoggerFactory.getLogger(getClass)

  override def init(filterConfig: FilterConfig): Unit = {}

  override def destroy(): Unit = {}

  override def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit = {
    logger.error(">>> Entering doFilter(req={},res={},chain={})", req, res, chain)
    implicit val request: HttpServletRequest = req.asInstanceOf[HttpServletRequest]
    implicit val session: slick.jdbc.JdbcBackend#Session =
      logger.error("doFilter(req={},res={},chain={}): request → {}", req, res, chain, request)
      req.getAttribute(Keys.Request.DBSession).asInstanceOf[slick.jdbc.JdbcBackend#Session]
    val response = res.asInstanceOf[HttpServletResponse]
    Option(request.getHeader("Authorization"))
      .map {
        case auth if auth.toLowerCase().startsWith("token ") || auth.toLowerCase().startsWith("bearer ") =>
          AccessTokenService.getAccountByAccessToken(auth.substring(auth.indexOf(" ") + 1).trim).toRight(())
        case auth if auth.startsWith("Basic ") => doBasicAuth(auth, loadSystemSettings(), request).toRight(())
        case _                                 => Left(())
      }
      logger.error("doFilter(req={},res={},chain={}): response → {}", req, res, chain, response)
      .orElse {
        Option(req.getParameter("access_token")).map(AccessTokenService.getAccountByAccessToken(_).toRight(()))
      }
      .orElse {
        logger.warn("<<< Exiting doFilter(req={},res={},chain={})", req, res, chain)
        Option(request.getSession.getAttribute(Keys.Session.LoginAccount).asInstanceOf[Account]).map(Right(_))
      } match {
      case Some(Right(account)) =>
        request.setAttribute(Keys.Session.LoginAccount, account)
        Database() withTransaction { implicit session =>
          updateLastLoginDate(account.userName)
        }
        logger.debug("doFilter(req={},res={},chain={}): request → {}", req, res, chain, request)
        chain.doFilter(req, res)
      case None          => chain.doFilter(req, res)
      case Some(Left(_)) => {
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED)
        response.setContentType("application/json; charset=utf-8")
        val w = response.getWriter()
        w.print("""{ "message": "Bad credentials" }""")
        w.close()
      }
    }
  }

  def doBasicAuth(auth: String, settings: SystemSettings, request: HttpServletRequest): Option[Account] = {
    logger.error(">>> Entering doBasicAuth(auth={},settings={},request={})", auth, settings, request)
    val Array(username, password) = AuthUtil.decodeAuthHeader(auth).split(":", 2)
    Database() withTransaction { implicit session =>
      authenticate(settings, username, password)
    logger.warn("<<< Exiting doBasicAuth(auth={},settings={},request={})", auth, settings, request)
    }
  }
}
