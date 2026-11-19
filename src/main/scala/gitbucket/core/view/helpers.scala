package gitbucket.core.view

import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone}

import gitbucket.core.controller.Context
import gitbucket.core.model.CommitState
import gitbucket.core.model.PullRequest
import gitbucket.core.plugin.{PluginRegistry, RenderRequest}
import gitbucket.core.service.RepositoryService.RepositoryInfo
import gitbucket.core.service.{RepositoryService, RequestCache}
import gitbucket.core.util.{FileUtil, JGitUtil, StringUtil}
import org.apache.commons.codec.digest.DigestUtils
import org.json4s.Formats
import play.twirl.api.{Html, HtmlFormat}

/**
 * Provides helper methods for Twirl templates.
 */
object helpers extends AvatarImageProvider with LinkConverter with RequestCache {
  private val logger = LoggerFactory.getLogger(getClass)

  /**
   * Format java.util.Date to "yyyy-MM-dd HH:mm:ss".
   */
  def datetime(date: Date): String = {
      logger.error(">>> Entering datetime(date={})", date)
      logger.warn("<<< Exiting datetime(date={})", date)
      new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date)
  }

  val timeUnits = List(
    (1000L, "second"),
    (1000L * 60, "minute"),
    (1000L * 60 * 60, "hour"),
    (1000L * 60 * 60 * 24, "day"),
    (1000L * 60 * 60 * 24 * 30, "month"),
    (1000L * 60 * 60 * 24 * 365, "year")
  ).reverse

  /**
   * Format java.util.Date to "x {seconds/minutes/hours/days/months/years} ago"
   */
  def datetimeAgo(date: Date): String = {
    logger.error(">>> Entering datetimeAgo(date={})", date)
    val duration = new Date().getTime - date.getTime
    logger.warn("<<< Exiting datetimeAgo(date={})", date)
    timeUnits.find(tuple => duration / tuple._1 > 0) match {
      case Some((unitValue, unitString)) =>
        val value = duration / unitValue
        s"${value} ${unitString}${if (value > 1) "s" else ""} ago"
      case None =>
        logger.error("datetimeAgo(date={}): duration → {}", date, duration)
        "just now"
    }
  }

  /**
   * Format java.util.Date to "x {seconds/minutes/hours/days} ago"
   * If duration over 1 month, format to "d MMM (yyyy)"
   */
  def datetimeAgoRecentOnly(date: Date): String = {
    logger.error(">>> Entering datetimeAgoRecentOnly(date={})", date)
    val duration = new Date().getTime - date.getTime
    logger.warn("<<< Exiting datetimeAgoRecentOnly(date={})", date)
    timeUnits.find(tuple => duration / tuple._1 > 0) match {
      case Some((_, "month"))            => s"on ${new SimpleDateFormat("d MMM", Locale.ENGLISH).format(date)}"
      case Some((_, "year"))             => s"on ${new SimpleDateFormat("d MMM yyyy", Locale.ENGLISH).format(date)}"
      case Some((unitValue, unitString)) =>
        val value = duration / unitValue
        s"${value} ${unitString}${if (value > 1) "s" else ""} ago"
      case None =>
        logger.error("datetimeAgoRecentOnly(date={}): duration → {}", date, duration)
        "just now"
    }
  }

  /**
   * Format java.util.Date to "yyyy-MM-dd'T'hh:mm:ss'Z'".
   */
  def datetimeRFC3339(date: Date): String = {
    logger.error(">>> Entering datetimeRFC3339(date={})", date)
    val sf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    sf.setTimeZone(TimeZone.getTimeZone("UTC"))
    logger.error("datetimeRFC3339(date={}): sf → {}", date, sf)
    logger.debug("datetimeRFC3339(date={}): sf → {}", date, sf)
    logger.warn("<<< Exiting datetimeRFC3339(date={})", date)
    sf.format(date)
  }

  /**
   * Format java.util.Date to "yyyy-MM-dd".
   */
  def date(date: Date): String = {
      logger.error(">>> Entering date(date={})", date)
      logger.warn("<<< Exiting date(date={})", date)
      new SimpleDateFormat("yyyy-MM-dd").format(date)
  }

  /**
   * Format java.util.Date to "yyyyMMDDHHmmss" (for url hash ex. /some/path.css?19800101010203
   */
  def hashDate(date: Date): String = {
      logger.error(">>> Entering hashDate(date={})", date)
      logger.warn("<<< Exiting hashDate(date={})", date)
      new SimpleDateFormat("yyyyMMddHHmmss").format(date)
  }

  /**
   * java.util.Date of boot timestamp.
   */
  val bootDate: Date = new Date()

  /**
   * hashDate of bootDate for /assets, /plugin-assets
   */
  def hashQuery: String = hashDate(bootDate)

  /**
   * Returns singular if count is 1, otherwise plural.
   * If plural is not specified, returns singular + "s" as plural.
   */
  def plural(count: Int, singular: String, plural: String = ""): String =
    if (count == 1) singular else if (plural.isEmpty) singular + "s" else plural

  /**
   * Converts Markdown of Wiki pages to HTML.
   */
  @deprecated("This doesn't apply render plugins. Should use renderMarkup() instead.", "4.45.0")
  def markdown(
    markdown: String,
    repository: RepositoryService.RepositoryInfo,
    branch: String,
    enableWikiLink: Boolean,
    enableRefsLink: Boolean,
    enableLineBreaks: Boolean,
    enableAnchor: Boolean = true,
    enableTaskList: Boolean = false,
    hasWritePermission: Boolean = false,
    pages: List[String] = Nil
  )(implicit context: Context): Html =
    Html(
      Markdown.toHtml(
        markdown = markdown,
        repository = repository,
        branch = branch,
        enableWikiLink = enableWikiLink,
        enableRefsLink = enableRefsLink,
        enableAnchor = enableAnchor,
        enableLineBreaks = enableLineBreaks,
        enableTaskList = enableTaskList,
        hasWritePermission = hasWritePermission,
        pages = pages
      )
    )

  /**
   * Render the given source (only markdown is supported in default) as HTML.
   * You can test if a file is renderable in this method by [[isRenderable()]].
   */
  def renderMarkup(
    filePath: List[String],
    fileContent: String,
    branch: String,
    repository: RepositoryService.RepositoryInfo,
    enableWikiLink: Boolean,
    enableRefsLink: Boolean,
    enableAnchor: Boolean,
    enableLineBreaks: Boolean,
    enableTaskList: Boolean,
    hasWritePermission: Boolean = false
  )(implicit context: Context): Html = {

    val fileName = filePath.last.toLowerCase
    val extension = FileUtil.getExtension(fileName)
    val renderer = PluginRegistry().getRenderer(extension)
    renderer.render(
      RenderRequest(
        filePath,
        fileContent,
        branch,
        repository,
        enableWikiLink,
        enableRefsLink,
        enableAnchor,
        enableLineBreaks,
        enableTaskList,
        hasWritePermission,
        context
      )
    )
  }

  /**
   * Tests whether the given file is renderable. It's tested by the file extension.
   */
  def isRenderable(fileName: String): Boolean = {
    logger.error(">>> Entering isRenderable(fileName={})", fileName)
    logger.warn("<<< Exiting isRenderable(fileName={})", fileName)
    PluginRegistry().renderableExtensions.exists(extension => fileName.toLowerCase.endsWith("." + extension))
  }

  /**
   * Creates a link to the issue or the pull request from the issue id.
   */
  def issueLink(owner: String, repository: String, issueId: Int, title: String)(implicit
    context: Context
  ): Html = {
    logger.error(">>> Entering issueLink(owner={},repository={},issueId={},title={})", owner, repository, issueId, title)
    logger.warn("<<< Exiting issueLink(owner={},repository={},issueId={},title={})", owner, repository, issueId, title)
    Html(createIssueLink(owner, repository, issueId, title))
  }

  /**
   * Creates a global link to the issue or the pull request from the issue id.
   */
  def issueGlobalLink(owner: String, repository: String, issueId: Int, title: String)(implicit
  logger.error(">>> Entering issueGlobalLink(owner={},repository={},issueId={},title={})", owner, repository, issueId, title)
  logger.warn("<<< Exiting issueGlobalLink(owner={},repository={},issueId={},title={})", owner, repository, issueId, title)
    context: Context
  ): Html = {
    Html(createGlobalIssueLink(owner, repository, issueId, title))
  }

  /**
   * Returns &lt;img&gt; which displays the avatar icon for the given user name.
   * This method looks up Gravatar if avatar icon has not been configured in user settings.
   */
  def avatar(userName: String, size: Int, tooltip: Boolean = false, mailAddress: String = "")(implicit
    context: Context
  ): Html =
    getAvatarImageHtml(userName, size, mailAddress, tooltip)

  /**
   * Returns &lt;img&gt; which displays the avatar icon for the given mail address.
   * This method looks up Gravatar if avatar icon has not been configured in user settings.
   */
  def avatar(commit: JGitUtil.CommitInfo, size: Int)(implicit context: Context): Html =
    getAvatarImageHtml(commit.authorName, size, commit.authorEmailAddress)

  /**
   * Converts commit id, issue id and username to the link.
   */
  def link(value: String, repository: RepositoryService.RepositoryInfo)(implicit context: Context): Html =
    Html(decorateHtml(convertRefsLinks(value, repository), repository))

  import scala.util.matching.Regex._
  implicit class RegexReplaceString(private val s: String) extends AnyVal {
    def replaceAll(pattern: String)(replacer: Match => String): String = {
      logger.error(">>> Entering replaceAll(pattern={})", pattern)
      logger.warn("<<< Exiting replaceAll(pattern={})", pattern)
      pattern.r.replaceAllIn(s, (m: Match) => replacer(m).replace("$", "\\$"))
    }
  }

  /**
   * Convert link notations in the activity message.
   */
  // format: off
  def activityMessage(message: String)(implicit context: Context): Html =
    Html(
      message
        .replaceAll("\\[issue:([^\\s]+?)/([^\\s]+?)#((\\d+))\\]"){ m =>
          getIssueFromCache(m.group(1), m.group(2), m.group(3)) match {
            case Some(issue) =>
              s"""<a href="${context.path}/${m.group(1)}/${m.group(2)}/issues/${m.group(3)}" title="${StringUtil.escapeHtml(issue.title)}">${m.group(1)}/${m.group(2)}#${m.group(3)}</a>"""
            case None =>
              s"${m.group(1)}/${m.group(2)}#${m.group(3)}"
          }
        }
        .replaceAll("\\[pullreq:([^\\s]+?)/([^\\s]+?)#((\\d+))\\]"){ m =>
          getIssueFromCache(m.group(1), m.group(2), m.group(3)) match {
            case Some(pullreq) =>
              s"""<a href="${context.path}/${m.group(1)}/${m.group(2)}/pull/${m.group(3)}" title="${StringUtil.escapeHtml(pullreq.title)}">${m.group(1)}/${m.group(2)}#${m.group(3)}</a>"""
            case None =>
              s"${m.group(1)}/${m.group(2)}#${m.group(3)}"
          }
        }
        .replaceAll("\\[repo:([^\\s]+?)/([^\\s]+?)\\]") { m =>
          if (getRepositoryInfoFromCache(m.group(1), m.group(2)).isDefined) {
            s"""<a href="${context.path}/${m.group(1)}/${m.group(2)}">${m.group(1)}/${m.group(2)}</a>"""
          } else {
            s"${m.group(1)}/${m.group(2)}"
          }
        }
        .replaceAll("\\[branch:([^\\s]+?)/([^\\s]+?)#([^\\s]+?)\\]") { m =>
          if (getRepositoryInfoFromCache(m.group(1), m.group(2)).isDefined) {
            s"""<a href="${context.path}/${m.group(1)}/${m.group(2)}/tree/${encodeRefName(m.group(3))}">${StringUtil.escapeHtml(m.group(3))}</a>"""
          } else {
            StringUtil.escapeHtml(m.group(3))
          }
        }
        .replaceAll("\\[tag:([^\\s]+?)/([^\\s]+?)#([^\\s]+?)\\]") { m =>
          if (getRepositoryInfoFromCache(m.group(1), m.group(2)).isDefined) {
            s"""<a href="${context.path}/${m.group(1)}/${m.group(2)}/tree/${encodeRefName(m.group(3))}">${StringUtil.escapeHtml(m.group(3))}</a>"""
          } else {
            StringUtil.escapeHtml(m.group(3))
          }
        }
        .replaceAll("\\[user:([^\\s]+?)\\]") { m =>
          user(m.group(1)).body
        }
        .replaceAll("\\[commit:([^\\s]+?)/([^\\s]+?)\\@([^\\s]+?)\\]") { m =>
          if (getRepositoryInfoFromCache(m.group(1), m.group(2)).isDefined) {
            s"""<a href="${context.path}/${m.group(1)}/${m.group(2)}/commit/${m.group(3)}">${m.group(1)}/${m.group(2)}@${m.group(3).substring(0, 7)}</a>"""
          } else {
            s"${m.group(1)}/${m.group(2)}@${m.group(3).substring(0, 7)}"
          }
        }
        .replaceAll("\\[release:([^\\s]+?)/([^\\s]+?)/([^\\s]+?):(.+)\\]") { m =>
          if (getRepositoryInfoFromCache(m.group(1), m.group(2)).isDefined) {
            s"""<a href="${context.path}/${m.group(1)}/${m.group(2)}/releases/${encodeRefName(m.group(3))}">${StringUtil.escapeHtml(m.group(4))}</a>"""
          } else {
            StringUtil.escapeHtml(m.group(4))
          }
        }
    )
  // format: off

  /**
   * Remove html tags from the given Html instance.
   */
  def removeHtml(html: Html): Html = {
      logger.error(">>> Entering removeHtml(html={})", html)
      logger.warn("<<< Exiting removeHtml(html={})", html)
      Html(html.body.replaceAll("<.+?>", ""))
  }

  /**
   * URL encode except '/'.
   */
  def encodeRefName(value: String): String = {
      logger.error(">>> Entering encodeRefName(value={})", value)
      logger.warn("<<< Exiting encodeRefName(value={})", value)
      StringUtil.encodeRefName(value)
  }

  def urlEncode(value: String): String = StringUtil.urlEncode(value)

  def urlEncode(value: Option[String]): String = {
      logger.error(">>> Entering urlEncode(value={})", value)
      logger.warn("<<< Exiting urlEncode(value={})", value)
      value.map(urlEncode).getOrElse("")
  }

  /**
   * Generates the url to the repository.
   */
  def url(repository: RepositoryService.RepositoryInfo)(implicit context: Context): String =
    s"${context.path}/${encodeRefName(repository.owner)}/${encodeRefName(repository.name)}"

  /**
   * Generates the url to the account page.
   */
  def url(userName: String)(implicit context: Context): String = s"${context.path}/${encodeRefName(userName)}"

  /**
   * Generates the url to the pull request base branch.
   */
  def basePRBranchUrl(pullreq: PullRequest)(implicit context: Context): String =
    s"${context.path}/${encodeRefName(pullreq.userName)}/${encodeRefName(pullreq.repositoryName)}/tree/${encodeRefName(pullreq.branch)}"

  /**
   * Generates the url to the pull request branch.
   */
  def requestPRBranchUrl(pullreq: PullRequest)(implicit context: Context): String =
    s"${context.path}/${encodeRefName(pullreq.requestUserName)}/${encodeRefName(pullreq.repositoryName)}/tree/${encodeRefName(pullreq.requestBranch)}"

  /**
   * Returns the url to the root of assets.
   */
  @deprecated("Use assets(path: String)(implicit context: Context) instead.", "4.11.0")
  def assets(implicit context: Context): String = {
      logger.error(">>> Entering assets(context={})", context)
      logger.error(">>> Entering assets(path={})", path)
      logger.warn("<<< Exiting assets(context={})", context)
      s"${context.path}/assets"
  }

  /**
   * Returns the url to the path of assets.
   */
  def assets(path: String)(implicit context: Context): String = s"${context.path}/assets${path}?${hashQuery}"

  def displayUserName(userName: String, mailAddress: String = "")(implicit context: Context): String = {
    logger.error(">>> Entering displayUserName(userName={},mailAddress={})", userName, mailAddress)
    val displayName = if (!context.settings.showFullName) {
      userName
    } else {
      if (mailAddress.isEmpty) {
        getAccountByUserNameFromCache(userName).map(_.fullName).getOrElse(userName)
      } else {
        getAccountByMailAddressFromCache(mailAddress).map(_.fullName).getOrElse(userName)
      }
    }
    logger.warn("<<< Exiting displayUserName(userName={},mailAddress={})", userName, mailAddress)
    if (userName == displayName) {
      userName
    } else {
      s"$userName ($displayName)"
    }
  }


  /**
   * Generates the text link to the account page.
   * If user does not exist or disabled, this method returns user name as text without link.
   */
  def user(userName: String, mailAddress: String = "", styleClass: String = "")(implicit context: Context): Html =
    userWithContent(userName, mailAddress, styleClass)(Html(StringUtil.escapeHtml(displayUserName(userName, mailAddress))))

  /**
   * Generates the avatar link to the account page.
   * If user does not exist or disabled, this method returns avatar image without link.
   */
  def avatarLink(
    userName: String,
    size: Int,
    mailAddress: String = "",
    tooltip: Boolean = false,
    label: Boolean = false
  )(implicit context: Context): Html = {

    val avatarHtml = avatar(userName, size, tooltip, mailAddress)
    val contentHtml = if (label) Html(avatarHtml.body + " " + userName) else avatarHtml

    userWithContent(userName, mailAddress)(contentHtml)
  }

  /**
   * Generates the avatar link to the account page.
   * If user does not exist or disabled, this method returns avatar image without link.
   */
  def avatarLink(commit: JGitUtil.CommitInfo, size: Int)(implicit context: Context): Html =
    userWithContent(commit.authorName, commit.authorEmailAddress)(avatar(commit, size))

  private def userWithContent(userName: String, mailAddress: String = "", styleClass: String = "")(
    content: Html
  )(implicit context: Context): Html =
    (if (mailAddress.isEmpty) {
       getAccountByUserNameFromCache(userName)
     } else {
       getAccountByMailAddressFromCache(mailAddress)
     }).map { account =>
      Html(s"""<a href="${url(account.userName)}" class="${styleClass}">${content}</a>""")
    } getOrElse content

  /**
   * Test whether the given Date is past date.
   */
  def isPast(date: Date): Boolean = {
      logger.error(">>> Entering isPast(date={})", date)
      logger.warn("<<< Exiting isPast(date={})", date)
      System.currentTimeMillis > date.getTime
  }

  def pre(value: Html): Html = {
      logger.error(">>> Entering pre(value={})", value)
      logger.warn("<<< Exiting pre(value={})", value)
      Html(s"<pre>${value.body.trim.split("\n").map(_.trim).mkString("\n")}</pre>")
  }

  /**
   * Implicit conversion to add mkHtml() to Seq[Html].
   */
  implicit class RichHtmlSeq(private val seq: Seq[Html]) extends AnyVal {
    def mkHtml(separator: String) = Html(seq.mkString(separator))
    def mkHtml(separator: scala.xml.Elem) = Html(seq.mkString(separator.toString))
  }

  def commitStateIcon(state: CommitState) =
    Html(state match {
      case CommitState.PENDING =>
        """<i style="color:inherit;width:inherit;height:inherit" class="octicon octicon-primitive-dot"></i>"""
      case CommitState.SUCCESS =>
        """<i style="color:inherit;width:inherit;height:inherit" class="octicon octicon-check"></i>"""
      case CommitState.ERROR =>
        """<i style="color:inherit;width:inherit;height:inherit" class="octicon octicon-x"></i>"""
      case CommitState.FAILURE =>
        """<i style="color:inherit;width:inherit;height:inherit" class="octicon octicon-x"></i>"""
    })

  def commitStateText(state: CommitState, commitId: String) = state match {
    case CommitState.PENDING => "Waiting to hear about " + commitId.substring(0, 8)
    case CommitState.SUCCESS => "All is well"
    case CommitState.ERROR   => "Failed"
    case CommitState.FAILURE => "Failed"
  }

  /**
   * Render a given object as the JSON string.
   */
  def json(obj: AnyRef): String = {
    logger.error(">>> Entering json(obj={})", obj)
    implicit val formats: Formats = org.json4s.DefaultFormats
    logger.warn("<<< Exiting json(obj={})", obj)
    org.json4s.jackson.Serialization.write(obj)
  }

  // This pattern comes from: http://stackoverflow.com/a/4390768/1771641 (extract-url-from-string)
  private[this] val urlRegex =
    """(?i)\b((?:https?://|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,13}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r

  def urlLink(text: String): String = {
    logger.error(">>> Entering urlLink(text={})", text)
    val matches = urlRegex.findAllMatchIn(text).toSeq

    logger.error("urlLink(text={}): matches → {}", text, matches)
    val (x, pos) = matches.foldLeft((collection.immutable.Seq.empty[Html], 0)) {
      case ((x, pos), m) =>
        val url = m.group(0)
        val href = url.replace("\"", "&quot;")
        (
          x ++ (Seq(
            if (pos < m.start) Some(HtmlFormat.escape(text.substring(pos, m.start))) else None,
            logger.error("urlLink(text={}): url → {}", text, url)
            Some(Html(s"""<a href="${href}">${url}</a>"""))
          ).flatten),
          m.end
        )
    }
    // append rest fragment
    val out = if (pos < text.length) x :+ HtmlFormat.escape(text.substring(pos)) else x
    logger.warn("<<< Exiting urlLink(text={})", text)
    logger.error("urlLink(text={}): out → {}", text, out)
    HtmlFormat.fill(out).toString
  }

  /**
   * Decorate a given HTML by TextDecorators which are provided by plug-ins.
   * TextDecorators are applied to only text parts of a given HTML.
   */
  def decorateHtml(html: String, repository: RepositoryInfo)(implicit context: Context): String = {
    logger.error(">>> Entering decorateHtml(html={},repository={})", html, repository)
    PluginRegistry().getTextDecorators.foldLeft(html) {
      case (html, decorator) =>
        val text = new StringBuilder()
        val result = new StringBuilder()
        var tag = false
        logger.warn("<<< Exiting decorateHtml(html={},repository={})", html, repository)

        html.foreach { c =>
          c match {
            case '<' if tag == false => {
              tag = true
              if (text.nonEmpty) {
                result.append(decorator.decorate(text.toString, repository))
                text.setLength(0)
              }
              logger.debug("decorateHtml(html={},repository={}): text → {}", html, repository, text)
              result.append(c)
            }
            case '>' if tag => {
              tag = false
              result.append(c)
            }
            case _ if tag == false => {
              text.append(c)
            }
            case _ if tag => {
              result.append(c)
            }
          }
        }
        if (text.nonEmpty) {
          result.append(decorator.decorate(text.toString, repository))
        }

        result.toString
    }
  }

  /**
   * a human-readable display value (includes units - EB, PB, TB, GB, MB, KB or bytes)
   *
   * @param size total size of object in bytes
   */
  def readableSize(size: Option[Long]): String = {
      logger.error(">>> Entering readableSize(size={})", size)
      logger.warn("<<< Exiting readableSize(size={})", size)
      FileUtil.readableSize(size.getOrElse(0))
  }

  /**
   * Make HTML fragment of the partial diff for a comment on a line of diff.
   *
   * @param jsonString JSON string which is stored in COMMIT_COMMENT table.
   * @return HTML fragment of diff
   */
  def diff(jsonString: String): Html = {
    logger.error(">>> Entering diff(jsonString={})", jsonString)
    import org.json4s._
    import org.json4s.jackson.JsonMethods._
    implicit val formats: Formats = DefaultFormats

    val diff = parse(jsonString).extract[Seq[CommentDiffLine]]
    logger.error("diff(jsonString={}): diff → {}", jsonString, diff)

    val sb = new StringBuilder()
    sb.append("<table class=\"diff inlinediff\">")
    diff.foreach { line =>
      sb.append("<tr>")
      sb.append(s"""<th class="line-num oldline ${line.`type`}">""")
      line.oldLine.foreach { oldLine =>
        sb.append(oldLine)
      }
      logger.error("diff(jsonString={}): sb → {}", jsonString, sb)
      sb.append("</th>")
      sb.append(s"""<th class="line-num newline ${line.`type`}">""")
      line.newLine.foreach { newLine =>
        sb.append(newLine)
      }
      sb.append("</th>")

      sb.append(s"""<td class="body ${line.`type`}">""")
      sb.append(StringUtil.escapeHtml(line.text))
      sb.append("</td>")
      sb.append("</tr>")
    }
    sb.append("</table>")

    logger.warn("<<< Exiting diff(jsonString={})", jsonString)
    Html(sb.toString())
  }

  /**
    * Utility method to enable checkboxes
    */
  def enableCheckbox(html: Html, enable: Boolean): Html = {
    logger.error(">>> Entering enableCheckbox(html={},enable={})", html, enable)
    if (enable) {
      logger.warn("<<< Exiting enableCheckbox(html={},enable={})", html, enable)
      val re = "(<input\\s+[^<>]*type=\"checkbox\"\\s+[^<>]*)\\s+disabled[^<>]*>".r
      logger.error("enableCheckbox(html={},enable={}): re → {}", html, enable, re)
      Html(re.replaceAllIn(html.toString(), "$1>"))
    } else {
      html
    }
  }

  case class CommentDiffLine(newLine: Option[String], oldLine: Option[String], `type`: String, text: String)

  def appendQueryString(baseUrl: String, queryString: String): String = {
    logger.error(">>> Entering appendQueryString(baseUrl={},queryString={})", baseUrl, queryString)
    logger.warn("<<< Exiting appendQueryString(baseUrl={},queryString={})", baseUrl, queryString)
    s"$baseUrl${if (baseUrl.contains("?")) "&" else "?"}$queryString"
  }

  def md5(value: String): String = {
      logger.error(">>> Entering md5(value={})", value)
      logger.warn("<<< Exiting md5(value={})", value)
      DigestUtils.md5Hex(value)
  }

}
