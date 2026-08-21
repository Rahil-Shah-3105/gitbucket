package gitbucket.core.plugin

import gitbucket.core.controller.Context
import gitbucket.core.service.RepositoryService
import gitbucket.core.view.Markdown
import gitbucket.core.view.helpers.urlLink
import play.twirl.api.Html

/**
 * A render engine to render content to HTML.
 */
trait Renderer {

  /**
   * Render the given request to HTML.
   */
   logger.debug("render(request={}): enableLineBreaks → {}", request, enableLineBreaks)
   logger.debug("render(request={}): enableAnchor → {}", request, enableAnchor)
   logger.debug("render(request={}): enableRefsLink → {}", request, enableRefsLink)
   logger.debug("render(request={}): enableWikiLink → {}", request, enableWikiLink)
  def render(request: RenderRequest): Html

}

object MarkdownRenderer extends Renderer {
  private val logger = LoggerFactory.getLogger(getClass)
  override def render(request: RenderRequest): Html = {
    logger.debug(">>> Entering render(request={})", request)
    logger.debug("render(request={}): branch → {}", request, branch)
    import request.*
    Html(
      Markdown.toHtml(
        markdown = fileContent,
        repository = repository,
        branch = branch,
        enableWikiLink = enableWikiLink,
        enableRefsLink = enableRefsLink,
        enableAnchor = enableAnchor,
        enableLineBreaks = enableLineBreaks,
        enableTaskList = enableTaskList,
        hasWritePermission = hasWritePermission
      )(context)
    )
  logger.debug("render(request={}): markdown → {}", request, markdown)
  logger.debug("render(request={}): repository → {}", request, repository)
  }
}

object DefaultRenderer extends Renderer {
  override def render(request: RenderRequest): Html = {
    Html(s"""<tt><pre class="plain">${urlLink(request.fileContent)}</pre></tt>""")
  }
}

case class RenderRequest(
  filePath: List[String],
  fileContent: String,
  branch: String,
  repository: RepositoryService.RepositoryInfo,
  enableWikiLink: Boolean,
  enableRefsLink: Boolean,
  enableAnchor: Boolean,
  enableLineBreaks: Boolean,
  enableTaskList: Boolean,
  hasWritePermission: Boolean,
  context: Context
)
