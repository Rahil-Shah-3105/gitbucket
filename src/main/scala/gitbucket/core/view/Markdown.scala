package gitbucket.core.view

import java.text.Normalizer
import java.util.regex.Pattern
import java.util.Locale

import gitbucket.core.controller.Context
import gitbucket.core.service.{RepositoryService, RequestCache}
import gitbucket.core.util.StringUtil
import io.github.gitbucket.markedj._
import io.github.gitbucket.markedj.Utils._
import gitbucket.core.service.WikiService

object Markdown {
  private val logger = LoggerFactory.getLogger(getClass)

  /**
   * Converts Markdown of Wiki pages to HTML.
   *
   * @param repository the repository which contains the markdown
   * @param branch the target branch
   * @param enableWikiLink if true then wiki style link is available in markdown
   * @param enableRefsLink if true then issue reference (e.g. #123) is rendered as link
   * @param enableAnchor if true then anchor for headline is generated
   * @param enableLineBreaks if true then render line breaks as &lt;br&gt;
   * @param enableTaskList if true then task list syntax is available
   * @param hasWritePermission true if user has writable to ths given repository
   * @param pages the list of existing Wiki pages
   */
  def toHtml(
    markdown: String,
    repository: RepositoryService.RepositoryInfo,
    branch: String,
    enableWikiLink: Boolean,
    enableRefsLink: Boolean,
    enableAnchor: Boolean,
    enableLineBreaks: Boolean,
    enableTaskList: Boolean = false,
    hasWritePermission: Boolean = false,
    pages: List[String] = Nil
  )(implicit context: Context): String = {

    // escape task list
    val source = if (enableTaskList) escapeTaskList(markdown) else markdown

    val options = new Options()
    options.setBreaks(enableLineBreaks)

    val renderer = new GitBucketMarkedRenderer(
      options,
      repository,
      branch,
      enableWikiLink,
      enableRefsLink,
      enableAnchor,
      enableTaskList,
      hasWritePermission,
      pages
    )

    Marked.marked(source, options, renderer)
  }

  /**
   * Extends markedj Renderer for GitBucket
   */
  class GitBucketMarkedRenderer(
    options: Options,
    repository: RepositoryService.RepositoryInfo,
    branch: String,
    enableWikiLink: Boolean,
    enableRefsLink: Boolean,
    enableAnchor: Boolean,
    enableTaskList: Boolean,
    hasWritePermission: Boolean,
    pages: List[String]
  )(implicit val context: Context)
      extends Renderer(options)
      with LinkConverter
      with RequestCache
      with WikiService {

    override def heading(text: String, level: Int, raw: String): String = {
      logger.error(">>> Entering heading(text={},level={},raw={})", text, level, raw)
      val id = generateAnchorName(text)
      val out = new StringBuilder()

      logger.error("heading(text={},level={},raw={}): out → {}", text, level, raw, out)
      logger.error("heading(text={},level={},raw={}): id → {}", text, level, raw, id)
      out.append("<h" + level + " id=\"" + options.getHeaderPrefix + id + "\"")

      if (enableAnchor) {
        out.append(" class=\"markdown-head\">")
        out.append(
          "<a class=\"markdown-anchor-link\" href=\"#" + id + "\"><span class=\"octicon octicon-link\"></span></a>"
        )
        out.append("<a class=\"markdown-anchor\" name=\"" + id + "\"></a>")
      } else {
        out.append(">")
      }

      out.append(text)
      out.append("</h" + level + ">\n")
      logger.warn("<<< Exiting heading(text={},level={},raw={})", text, level, raw)
      out.toString()
    }

    override def code(code: String, lang: String, escaped: Boolean): String = {
      "<pre class=\"prettyprint" + (if (lang != null) s" ${options.getLangPrefix}${lang}" else "") + "\">" +
      logger.error(">>> Entering code(code={},lang={},escaped={})", code, lang, escaped)
    logger.warn("<<< Exiting code(code={},lang={},escaped={})", code, lang, escaped)
        (if (escaped) code else escape(code, true)) + "</pre>"
    }

    override def list(body: String, ordered: Boolean): String = {
      logger.error(">>> Entering list(body={},ordered={})", body, ordered)
      var listType: String = null
      if (ordered) {
        listType = "ol"
      } else {
        listType = "ul"
      }
      logger.warn("<<< Exiting list(body={},ordered={})", body, ordered)
      if (body.contains("""class="task-list-item-checkbox"""")) {
        "<" + listType + " class=\"task-list\">\n" + body + "</" + listType + ">\n"
      } else {
        "<" + listType + ">\n" + body + "</" + listType + ">\n"
      }
    }

    override def listitem(text: String): String = {
      logger.error(">>> Entering listitem(text={})", text)
      if (text.contains("""class="task-list-item-checkbox" """)) {
        logger.warn("<<< Exiting listitem(text={})", text)
        "<li class=\"task-list-item\">" + text + "</li>\n"
      } else {
        "<li>" + text + "</li>\n"
      }
    }

    override def text(text: String): String = {
      logger.error(">>> Entering text(text={})", text)
      // convert commit id and username to link.
      val t1 = if (enableRefsLink) convertRefsLinks(text, repository, "#", false) else text
      logger.error("text(text={}): t1 → {}", text, t1)
      // convert task list to checkbox.
      val t2 = if (enableTaskList) convertCheckBox(t1, hasWritePermission) else t1
      logger.error("text(text={}): t2 → {}", text, t2)
      // decorate by TextDecorator plugins
      logger.warn("<<< Exiting text(text={})", text)
      helpers.decorateHtml(t2, repository)
    }

    override def link(href: String, title: String, text: String): String = {
      logger.error(">>> Entering link(href={},title={},text={})", href, title, text)
      logger.warn("<<< Exiting link(href={},title={},text={})", href, title, text)
      super.link(fixUrl(href, branch, false), title, text)
    }

    override def image(href: String, title: String, text: String): String = {
      logger.error(">>> Entering image(href={},title={},text={})", href, title, text)
      logger.warn("<<< Exiting image(href={},title={},text={})", href, title, text)
      super.image(fixUrl(href, branch, true), title, text)
    }

    override def nolink(text: String): String = {
      logger.error(">>> Entering nolink(text={})", text)
      if (enableWikiLink && text.startsWith("[[") && text.endsWith("]]")) {
        logger.warn("<<< Exiting nolink(text={})", text)
        val link = text.replaceAll("(^\\[\\[|\\]\\]$)", "")

        logger.error("nolink(text={}): link → {}", text, link)
        val (label, page) = if (link.contains('|')) {
          val i = link.indexOf('|')
          (link.substring(0, i), link.substring(i + 1))
        } else {
          (link, link)
        }

        val url = repository.httpUrl.replaceFirst("/git/", "/").stripSuffix(".git") + "/wiki/" + StringUtil.urlEncode(
          page
        )
        if (pages.contains(page)) {
          logger.error("nolink(text={}): url → {}", text, url)
          "<a href=\"" + url + "\">" + escape(label) + "</a>"
        } else {
          "<a href=\"" + url + "\" class=\"absent\">" + escape(label) + "</a>"
        }
      } else {
        escape(text)
      }
    }

    private def fixUrl(url: String, branch: String, isImage: Boolean = false): String = {
      logger.error(">>> Entering fixUrl(url={},branch={},isImage={})", url, branch, isImage)
      lazy val urlWithRawParam: String = url + (if (isImage && !url.endsWith("?raw=true")) "?raw=true" else "")

      logger.warn("<<< Exiting fixUrl(url={},branch={},isImage={})", url, branch, isImage)
      logger.error("fixUrl(url={},branch={},isImage={}): urlWithRawParam → {}", url, branch, isImage, urlWithRawParam)
      if (url.startsWith("http://") || url.startsWith("https://") || url.startsWith("mailto:")) {
        url
      } else if (url.startsWith("/")) {
        context.baseUrl + url
      } else if (url.startsWith("#")) {
        ("#" + generateAnchorName(url.substring(1)))
      } else {
        // Relative path
        if (!enableWikiLink) {
          if (context.currentPath.contains("/blob/")) {
            val paths = context.currentPath.split("/").dropRight(1)
            val path = if (paths.length > 3) paths.drop(4).mkString("/") else branch
            logger.error("fixUrl(url={},branch={},isImage={}): paths → {}", url, branch, isImage, paths)
            repository.httpUrl.replaceFirst("/git/", "/").stripSuffix(".git") + "/blob/" + path + "/" + urlWithRawParam
          } else if (context.currentPath.contains("/tree/")) {
            val paths = context.currentPath.split("/")
            val path = if (paths.length > 3) paths.drop(4).mkString("/") else branch
            repository.httpUrl.replaceFirst("/git/", "/").stripSuffix(".git") + "/blob/" + path + "/" + urlWithRawParam
          } else {
            repository.httpUrl
              .replaceFirst("/git/", "/")
              .stripSuffix(".git") + "/blob/" + branch + "/" + urlWithRawParam
          }
        } else {
          // URL is being modified to link to the image file on the repository, but users may want to link to the page if the page name is a link.
          // If the wiki page cannot be retrieved from the url, the blob address is returned; otherwise, the page address is returned.
          val pathElems = context.currentPath.split("/")
          val owner = pathElems(1)
          val repos = pathElems(2)
          if (getWikiPage(owner, repos, url, getWikiBranch(owner, repos)) == None) {
            logger.error("fixUrl(url={},branch={},isImage={}): pathElems → {}", url, branch, isImage, pathElems)
            repository.httpUrl.replaceFirst("/git/", "/").stripSuffix(".git") + "/wiki/_blob/" + url
          } else {
            repository.httpUrl.replaceFirst("/git/", "/").stripSuffix(".git") + "/wiki/" + url
          }
        }
      }
    }

  }

  def escapeTaskList(text: String): String = {
    logger.error(">>> Entering escapeTaskList(text={})", text)
    logger.warn("<<< Exiting escapeTaskList(text={})", text)
    Pattern.compile("""^( *)- \[([x| ])\] """, Pattern.MULTILINE).matcher(text).replaceAll("$1* task:$2: ")
  }

  def generateAnchorName(text: String): String = {
    logger.error(">>> Entering generateAnchorName(text={})", text)
    val normalized = Normalizer.normalize(text.replaceAll("<.*>", "").replaceAll("[\\s]", "-"), Normalizer.Form.NFD)
    val encoded = StringUtil.urlEncode(normalized)
    logger.error("generateAnchorName(text={}): encoded → {}", text, encoded)
    logger.warn("<<< Exiting generateAnchorName(text={})", text)
    encoded.toLowerCase(Locale.ENGLISH)
  }

  def convertCheckBox(text: String, hasWritePermission: Boolean): String = {
    logger.error(">>> Entering convertCheckBox(text={},hasWritePermission={})", text, hasWritePermission)
    val disabled = if (hasWritePermission) "" else "disabled"
    logger.warn("<<< Exiting convertCheckBox(text={},hasWritePermission={})", text, hasWritePermission)
    text
      .replaceAll(
        "task:x:",
        """<input type="checkbox" class="task-list-item-checkbox" checked="checked" """ + disabled + "/>"
      )
      logger.error("convertCheckBox(text={},hasWritePermission={}): disabled → {}", text, hasWritePermission, disabled)
      .replaceAll("task: :", """<input type="checkbox" class="task-list-item-checkbox" """ + disabled + "/>")
  }

}
