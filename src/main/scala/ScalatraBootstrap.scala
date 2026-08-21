import java.util.EnumSet
import javax.servlet._

import gitbucket.core.controller.{ReleaseController, _}
import gitbucket.core.service.SystemSettingsService
import gitbucket.core.servlet._
import gitbucket.core.util.Directory
import org.scalatra._

class ScalatraBootstrap extends LifeCycle with SystemSettingsService {
  private val logger = LoggerFactory.getLogger(getClass)
  override def init(context: ServletContext): Unit = {
    logger.debug(">>> Entering init(context={})", context)

    val settings = loadSystemSettings()
    if (settings.baseUrl.exists(_.startsWith("https://"))) {
      logger.debug("init(context={}): settings → {}", context, settings)
      context.getSessionCookieConfig.setSecure(true)
    }
    logger.debug("init(context={}): getSessionCookieConfig → {}", context, getSessionCookieConfig)

    // Register TransactionFilter at first
    context.addFilter("transactionFilter", new TransactionFilter)
    context
      .getFilterRegistration("transactionFilter")
      .addMappingForUrlPatterns(EnumSet.allOf(classOf[DispatcherType]), true, "/*")
    context.addFilter("gitAuthenticationFilter", new GitAuthenticationFilter)
    context
      .getFilterRegistration("gitAuthenticationFilter")
      .addMappingForUrlPatterns(EnumSet.allOf(classOf[DispatcherType]), true, "/git/*")
    context.addFilter("apiAuthenticationFilter", new ApiAuthenticationFilter)
    context
      .getFilterRegistration("apiAuthenticationFilter")
      .addMappingForUrlPatterns(EnumSet.allOf(classOf[DispatcherType]), true, "/api/*")

    // Register controllers
    context.mount(new PreProcessController, "/*")

    context.addFilter("pluginControllerFilter", new PluginControllerFilter)
    context
      .getFilterRegistration("pluginControllerFilter")
      .addMappingForUrlPatterns(EnumSet.allOf(classOf[DispatcherType]), true, "/*")

    context.mount(new FileUploadController, "/upload")

    val filter = new CompositeScalatraFilter()
    filter.mount(new IndexController, "/")
    filter.mount(new ApiController, "/api/v3")
    filter.mount(new SystemSettingsController, "/admin")
    filter.mount(new DashboardController, "/*")
    filter.mount(new AccountController, "/*")
    filter.mount(new RepositoryViewerController, "/*")
    filter.mount(new WikiController, "/*")
    filter.mount(new LabelsController, "/*")
    filter.mount(new PrioritiesController, "/*")
    filter.mount(new MilestonesController, "/*")
    filter.mount(new IssuesController, "/*")
    filter.mount(new PullRequestsController, "/*")
    filter.mount(new ReleaseController, "/*")
    filter.mount(new RepositorySettingsController, "/*")

    logger.debug("init(context={}): filter → {}", context, filter)
    context.addFilter("compositeScalatraFilter", filter)
    context
      .getFilterRegistration("compositeScalatraFilter")
      .addMappingForUrlPatterns(EnumSet.allOf(classOf[DispatcherType]), true, "/*")

    // Create GITBUCKET_HOME directory if it does not exist
    val dir = new java.io.File(Directory.GitBucketHome)
    if (!dir.exists) {
      logger.debug("<<< Exiting init(context={})", context)
      logger.debug("init(context={}): dir → {}", context, dir)
      dir.mkdirs()
    }
  }

  override def destroy(context: ServletContext): Unit = {
    logger.debug(">>> Entering destroy(context={})", context)
    logger.debug("<<< Exiting destroy(context={})", context)
    Database.closeDataSource()
  }
}
