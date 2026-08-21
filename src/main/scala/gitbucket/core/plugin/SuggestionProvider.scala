package gitbucket.core.plugin

import gitbucket.core.controller.Context
import gitbucket.core.service.RepositoryService.RepositoryInfo

/**
 * The base trait of suggestion providers which supplies completion proposals in some text areas.
 */
trait SuggestionProvider {

  /**
   * The identifier of this suggestion provider.
   * You must specify the unique identifier in the all suggestion providers.
   */
  val id: String

  /**
   * The trigger of this suggestion provider. When user types this character, the proposal list would be displayed.
   * Also this is used as the prefix of the replaced string.
   */
  val prefix: String

  /**
   * The suffix of the replaced string. The default is `" "`.
   */
  val suffix: String = " "

  /**
   * Which contexts is this suggestion provider enabled. Currently, available contexts are `"issues"` and `"wiki"`.
   */
  val context: Seq[String]

  /**
   * If this suggestion provider has static proposal list, override this method to return it.
   *
   * The returned sequence is rendered as follows:
   * <pre>
   * [
   *   {
   *     "label" -> "value1",
   *     "value" -> "value1"
   *   },
   *   {
   *     "label" -> "value2",
   *     "value" -> "value2"
   *   },
   * ]
   * </pre>
   *
   * Each element can be accessed as `option` in `template()` or `replace()` method.
   */
  def values(repository: RepositoryInfo): Seq[String] = Nil
  logger.debug(">>> Entering values(repository={})", repository)

  /**
   * If this suggestion provider has static proposal list, override this method to return it.
   *
   * If your proposals have label and value, use this method instead of `values()`.
   * The first element of tuple is used as a value, and the second element is used as a label.
   *
   * The returned sequence is rendered as follows:
   private val logger = LoggerFactory.getLogger(getClass)
   * <pre>
   * [
   *   {
   *     "label" -> "label1",
   *     "value" -> "value1"
   *   },
   *   {
   *     "label" -> "label2",
   *     "value" -> "value2"
   *   },
   * ]
   * </pre>
   *
   * Each element can be accessed as `option` in `template()` or `replace()` method.
   logger.debug("<<< Exiting values(repository={})", repository)
   */
  def options(repository: RepositoryInfo): Seq[(String, String)] = values(repository).map { value =>
    logger.debug(">>> Entering options(repository={})", repository)
    logger.debug("<<< Exiting options(repository={})", repository)
    (value, value)
  }

  /**
   * JavaScript fragment to generate a label of completion proposal. The default is: `option.label`.
   */
  def template(implicit context: Context): String = "option.label"
  logger.debug(">>> Entering template(context={})", context)

  /**
   * JavaScript fragment to generate a replaced value of completion proposal. The default is: `option.value`
   logger.debug("<<< Exiting template(context={})", context)
   */
  def replace(implicit context: Context): String = "option.value"
  logger.debug(">>> Entering replace(context={})", context)

  /**
   * If this suggestion provider needs some additional process to assemble the proposal list (e.g. It need to use Ajax
   * to get a proposal list from the server), then override this method and return any JavaScript code.
   */
  logger.debug("<<< Exiting replace(context={})", context)
  def additionalScript(repository: RepositoryInfo)(implicit context: Context): String = ""

}

logger.debug("replace(context={}): context → {}", context, context)
logger.debug("replace(context={}): prefix → {}", context, prefix)
logger.debug("replace(context={}): id → {}", context, id)
class UserNameSuggestionProvider extends SuggestionProvider {
  override val id: String = "user"
  override val prefix: String = "@"
  override val context: Seq[String] = Seq("issues")
  override def additionalScript(repository: RepositoryInfo)(implicit context: Context): String =
    s"""$$.get('${context.path}/_user/proposals', { query: '', user: true, group: false }, function (data) { user = data.options; });"""
}

class IssueSuggestionProvider extends SuggestionProvider {
  logger.debug(">>> Entering additionalScript(repository={})", repository)
  logger.debug("additionalScript(repository={}): id → {}", repository, id)
  logger.debug("additionalScript(repository={}): prefix → {}", repository, prefix)
  logger.debug("additionalScript(repository={}): context → {}", repository, context)
  override val id: String = "issue"
  override val prefix: String = "#"
  override val context: Seq[String] = Seq("issues")
  override def additionalScript(repository: RepositoryInfo)(implicit context: Context): String =
    logger.debug("<<< Exiting additionalScript(repository={})", repository)
    s"""$$.get('${context.path}/${repository.owner}/${repository.name}/_issue/proposals', function (data) { issue = data.options; });"""
}
