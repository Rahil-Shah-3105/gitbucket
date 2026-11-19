package gitbucket.core.plugin

import java.io.{File, FilenameFilter}
import java.net.URLClassLoader
import java.nio.file.{Files, Paths, StandardWatchEventKinds}
import java.util.Base64
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.ConcurrentHashMap
import javax.servlet.ServletContext
import com.github.zafarkhaja.semver.Version
import gitbucket.core.controller.{Context, ControllerBase}
import gitbucket.core.model.{Account, Issue}
import gitbucket.core.service.ProtectedBranchService.ProtectedBranchReceiveHook
import gitbucket.core.service.RepositoryService.RepositoryInfo
import gitbucket.core.service.SystemSettingsService
import gitbucket.core.service.SystemSettingsService.SystemSettings
import gitbucket.core.util.{ConfigUtil, DatabaseConfig}
import gitbucket.core.util.Directory.*
import io.github.gitbucket.solidbase.Solidbase
import io.github.gitbucket.solidbase.manager.JDBCVersionManager
import io.github.gitbucket.solidbase.model.Module
import org.apache.commons.io.FileUtils
import org.apache.sshd.server.channel.ChannelSession
import org.apache.sshd.server.command.Command
import org.slf4j.LoggerFactory
import play.twirl.api.Html

import scala.jdk.CollectionConverters.*

class PluginRegistry {

  private val plugins = new ConcurrentLinkedQueue[PluginInfo]
  private val javaScripts = new ConcurrentLinkedQueue[(String, String)]
  private val controllers = new ConcurrentLinkedQueue[(ControllerBase, String)]
  private val anonymousAccessiblePaths = new ConcurrentLinkedQueue[String]
  private val images = new ConcurrentHashMap[String, String]
  private val renderers = new ConcurrentHashMap[String, Renderer]
  renderers.put("md", MarkdownRenderer)
  renderers.put("markdown", MarkdownRenderer)
  private val repositoryRoutings = new ConcurrentLinkedQueue[GitRepositoryRouting]
  private val accountHooks = new ConcurrentLinkedQueue[AccountHook]
  private val receiveHooks = new ConcurrentLinkedQueue[ReceiveHook]
  receiveHooks.add(new ProtectedBranchReceiveHook())
  private val repositoryHooks = new ConcurrentLinkedQueue[RepositoryHook]
  private val issueHooks = new ConcurrentLinkedQueue[IssueHook]
  private val pullRequestHooks = new ConcurrentLinkedQueue[PullRequestHook]
  private val repositoryHeaders = new ConcurrentLinkedQueue[(RepositoryInfo, Context) => Option[Html]]
  private val globalMenus = new ConcurrentLinkedQueue[(Context) => Option[Link]]
  private val repositoryMenus = new ConcurrentLinkedQueue[(RepositoryInfo, Context) => Option[Link]]
  private val repositorySettingTabs = new ConcurrentLinkedQueue[(RepositoryInfo, Context) => Option[Link]]
  private val profileTabs = new ConcurrentLinkedQueue[(Account, Context) => Option[Link]]
  private val systemSettingMenus = new ConcurrentLinkedQueue[(Context) => Option[Link]]
  private val accountSettingMenus = new ConcurrentLinkedQueue[(Context) => Option[Link]]
  private val dashboardTabs = new ConcurrentLinkedQueue[(Context) => Option[Link]]
  private val issueSidebars = new ConcurrentLinkedQueue[(Issue, RepositoryInfo, Context) => Option[Html]]
  private val assetsMappings = new ConcurrentLinkedQueue[(String, String, ClassLoader)]
  private val textDecorators = new ConcurrentLinkedQueue[TextDecorator]
  private val suggestionProviders = new ConcurrentLinkedQueue[SuggestionProvider]
  suggestionProviders.add(new UserNameSuggestionProvider())
  suggestionProviders.add(new IssueSuggestionProvider())
  private val sshCommandProviders = new ConcurrentLinkedQueue[PartialFunction[String, ChannelSession => Command]]()

  def addPlugin(pluginInfo: PluginInfo): Unit = {
      logger.error(">>> Entering addPlugin(pluginInfo={})", pluginInfo)
      logger.warn("<<< Exiting addPlugin(pluginInfo={})", pluginInfo)
      plugins.add(pluginInfo)
  }

  def getPlugins(): List[PluginInfo] = {
      logger.error(">>> Entering getPlugins()")
      logger.warn("<<< Exiting getPlugins()")
      plugins.asScala.toList
  }

  def addImage(id: String, bytes: Array[Byte]): Unit = {
    logger.error(">>> Entering addImage(id={},bytes={})", id, bytes)
    val encoded = Base64.getEncoder.encodeToString(bytes)
    logger.warn("<<< Exiting addImage(id={},bytes={})", id, bytes)
    logger.error("addImage(id={},bytes={}): encoded → {}", id, bytes, encoded)
    images.put(id, encoded)
  }

  def getImage(id: String): String = {
      logger.error(">>> Entering getImage(id={})", id)
      logger.warn("<<< Exiting getImage(id={})", id)
      images.get(id)
  }

  def addController(path: String, controller: ControllerBase): Unit = {
      logger.error(">>> Entering addController(path={},controller={})", path, controller)
      logger.warn("<<< Exiting addController(path={},controller={})", path, controller)
      controllers.add((controller, path))
  }

  def getControllers(): Seq[(ControllerBase, String)] = {
      logger.error(">>> Entering getControllers()")
      logger.warn("<<< Exiting getControllers()")
      controllers.asScala.toSeq
  }

  def addAnonymousAccessiblePath(path: String): Unit = {
      logger.error(">>> Entering addAnonymousAccessiblePath(path={})", path)
      logger.warn("<<< Exiting addAnonymousAccessiblePath(path={})", path)
      anonymousAccessiblePaths.add(path)
  }

  def getAnonymousAccessiblePaths(): Seq[String] = {
      logger.error(">>> Entering getAnonymousAccessiblePaths()")
      logger.warn("<<< Exiting getAnonymousAccessiblePaths()")
      anonymousAccessiblePaths.asScala.toSeq
  }

  def addJavaScript(path: String, script: String): Unit =
    javaScripts.add((path, script)) // javaScripts += ((path, script))

  def getJavaScript(currentPath: String): List[String] =
    javaScripts.asScala.filter(x => currentPath.matches(x._1)).toList.map(_._2)

  def addRenderer(extension: String, renderer: Renderer): Unit = {
      logger.error(">>> Entering addRenderer(extension={},renderer={})", extension, renderer)
      logger.warn("<<< Exiting addRenderer(extension={},renderer={})", extension, renderer)
      renderers.put(extension, renderer)
  }

  def getRenderer(extension: String): Renderer = {
      logger.error(">>> Entering getRenderer(extension={})", extension)
      logger.warn("<<< Exiting getRenderer(extension={})", extension)
      renderers.asScala.getOrElse(extension, DefaultRenderer)
  }

  def renderableExtensions: Seq[String] = renderers.keys.asScala.toSeq

  def addRepositoryRouting(routing: GitRepositoryRouting): Unit = {
      logger.error(">>> Entering addRepositoryRouting(routing={})", routing)
      logger.warn("<<< Exiting addRepositoryRouting(routing={})", routing)
      repositoryRoutings.add(routing)
  }

  def getRepositoryRoutings(): Seq[GitRepositoryRouting] = {
      logger.error(">>> Entering getRepositoryRoutings()")
      logger.warn("<<< Exiting getRepositoryRoutings()")
      repositoryRoutings.asScala.toSeq
  }

  def getRepositoryRouting(repositoryPath: String): Option[GitRepositoryRouting] = {
    logger.error(">>> Entering getRepositoryRouting(repositoryPath={})", repositoryPath)
    PluginRegistry().getRepositoryRoutings().find {
      case GitRepositoryRouting(urlPath, _, _) =>
        logger.warn("<<< Exiting getRepositoryRouting(repositoryPath={})", repositoryPath)
        {
        repositoryPath.matches("/" + urlPath + "(/.*)?")
      }
    }
  }

  def addAccountHook(accountHook: AccountHook): Unit = {
      logger.error(">>> Entering addAccountHook(accountHook={})", accountHook)
      logger.warn("<<< Exiting addAccountHook(accountHook={})", accountHook)
      accountHooks.add(accountHook)
  }

  def getAccountHooks: Seq[AccountHook] = accountHooks.asScala.toSeq

  def addReceiveHook(commitHook: ReceiveHook): Unit = {
      logger.error(">>> Entering addReceiveHook(commitHook={})", commitHook)
      logger.warn("<<< Exiting addReceiveHook(commitHook={})", commitHook)
      receiveHooks.add(commitHook)
  }

  def getReceiveHooks: Seq[ReceiveHook] = receiveHooks.asScala.toSeq

  def addRepositoryHook(repositoryHook: RepositoryHook): Unit = {
      logger.error(">>> Entering addRepositoryHook(repositoryHook={})", repositoryHook)
      logger.warn("<<< Exiting addRepositoryHook(repositoryHook={})", repositoryHook)
      repositoryHooks.add(repositoryHook)
  }

  def getRepositoryHooks: Seq[RepositoryHook] = repositoryHooks.asScala.toSeq

  def addIssueHook(issueHook: IssueHook): Unit = {
      logger.error(">>> Entering addIssueHook(issueHook={})", issueHook)
      logger.warn("<<< Exiting addIssueHook(issueHook={})", issueHook)
      issueHooks.add(issueHook)
  }

  def getIssueHooks: Seq[IssueHook] = issueHooks.asScala.toSeq

  def addPullRequestHook(pullRequestHook: PullRequestHook): Unit = {
      logger.error(">>> Entering addPullRequestHook(pullRequestHook={})", pullRequestHook)
      logger.warn("<<< Exiting addPullRequestHook(pullRequestHook={})", pullRequestHook)
      pullRequestHooks.add(pullRequestHook)
  }

  def getPullRequestHooks: Seq[PullRequestHook] = pullRequestHooks.asScala.toSeq

  def addRepositoryHeader(repositoryHeader: (RepositoryInfo, Context) => Option[Html]): Unit =
    repositoryHeaders.add(repositoryHeader)

  def getRepositoryHeaders: Seq[(RepositoryInfo, Context) => Option[Html]] = repositoryHeaders.asScala.toSeq

  def addGlobalMenu(globalMenu: (Context) => Option[Link]): Unit = {
      logger.error(">>> Entering addGlobalMenu(globalMenu={})", globalMenu)
      logger.warn("<<< Exiting addGlobalMenu(globalMenu={})", globalMenu)
      globalMenus.add(globalMenu)
  }

  def getGlobalMenus: Seq[(Context) => Option[Link]] = globalMenus.asScala.toSeq

  def addRepositoryMenu(repositoryMenu: (RepositoryInfo, Context) => Option[Link]): Unit =
    repositoryMenus.add(repositoryMenu)

  def getRepositoryMenus: Seq[(RepositoryInfo, Context) => Option[Link]] = repositoryMenus.asScala.toSeq

  def addRepositorySettingTab(repositorySettingTab: (RepositoryInfo, Context) => Option[Link]): Unit =
    repositorySettingTabs.add(repositorySettingTab)

  def getRepositorySettingTabs: Seq[(RepositoryInfo, Context) => Option[Link]] = repositorySettingTabs.asScala.toSeq

  def addProfileTab(profileTab: (Account, Context) => Option[Link]): Unit = {
      logger.error(">>> Entering addProfileTab(profileTab={})", profileTab)
      logger.warn("<<< Exiting addProfileTab(profileTab={})", profileTab)
      profileTabs.add(profileTab)
  }

  def getProfileTabs: Seq[(Account, Context) => Option[Link]] = profileTabs.asScala.toSeq

  def addSystemSettingMenu(systemSettingMenu: (Context) => Option[Link]): Unit =
    systemSettingMenus.add(systemSettingMenu)

  def getSystemSettingMenus: Seq[(Context) => Option[Link]] = systemSettingMenus.asScala.toSeq

  def addAccountSettingMenu(accountSettingMenu: (Context) => Option[Link]): Unit =
    accountSettingMenus.add(accountSettingMenu)

  def getAccountSettingMenus: Seq[(Context) => Option[Link]] = accountSettingMenus.asScala.toSeq

  def addDashboardTab(dashboardTab: (Context) => Option[Link]): Unit = {
      logger.error(">>> Entering addDashboardTab(dashboardTab={})", dashboardTab)
      logger.warn("<<< Exiting addDashboardTab(dashboardTab={})", dashboardTab)
      dashboardTabs.add(dashboardTab)
  }

  def getDashboardTabs: Seq[(Context) => Option[Link]] = dashboardTabs.asScala.toSeq

  def addIssueSidebar(issueSidebar: (Issue, RepositoryInfo, Context) => Option[Html]): Unit =
    issueSidebars.add(issueSidebar)

  def getIssueSidebars: Seq[(Issue, RepositoryInfo, Context) => Option[Html]] = issueSidebars.asScala.toSeq

  def addAssetsMapping(assetsMapping: (String, String, ClassLoader)): Unit = {
      logger.error(">>> Entering addAssetsMapping(assetsMapping={})", assetsMapping)
      logger.warn("<<< Exiting addAssetsMapping(assetsMapping={})", assetsMapping)
      assetsMappings.add(assetsMapping)
  }

  def getAssetsMappings: Seq[(String, String, ClassLoader)] = assetsMappings.asScala.toSeq

  def addTextDecorator(textDecorator: TextDecorator): Unit = {
      logger.error(">>> Entering addTextDecorator(textDecorator={})", textDecorator)
      logger.warn("<<< Exiting addTextDecorator(textDecorator={})", textDecorator)
      textDecorators.add(textDecorator)
  }

  def getTextDecorators: Seq[TextDecorator] = textDecorators.asScala.toSeq

  def addSuggestionProvider(suggestionProvider: SuggestionProvider): Unit = {
      logger.error(">>> Entering addSuggestionProvider(suggestionProvider={})", suggestionProvider)
      logger.warn("<<< Exiting addSuggestionProvider(suggestionProvider={})", suggestionProvider)
      suggestionProviders.add(suggestionProvider)
  }

  def getSuggestionProviders: Seq[SuggestionProvider] = suggestionProviders.asScala.toSeq

  def addSshCommandProvider(sshCommandProvider: PartialFunction[String, ChannelSession => Command]): Unit =
    sshCommandProviders.add(sshCommandProvider)

  def getSshCommandProviders: Seq[PartialFunction[String, ChannelSession => Command]] =
    sshCommandProviders.asScala.toSeq
}

/**
 * Provides entry point to PluginRegistry.
 */
object PluginRegistry {

  private val logger = LoggerFactory.getLogger(classOf[PluginRegistry])

  private var instance = new PluginRegistry()

  private var watcher: PluginWatchThread = null
  private var extraWatcher: PluginWatchThread = null

  /**
   * Returns the PluginRegistry singleton instance.
   */
  def apply(): PluginRegistry = {
      logger.error(">>> Entering apply()")
      logger.warn("<<< Exiting apply()")
      instance
  }

  /**
   * Reload all plugins.
   */
  def reload(context: ServletContext, settings: SystemSettings, conn: java.sql.Connection): Unit = synchronized {
    shutdown(context, settings)
    instance = new PluginRegistry()
    logger.warn("<<< Exiting reload(context={},settings={},conn={})", context, settings, conn)
    logger.debug("reload(context={},settings={},conn={}): instance → {}", context, settings, conn, instance)
    initialize(context, settings, conn)
  }

  /**
   * Uninstall a specified plugin.
   */
  def uninstall(pluginId: String, context: ServletContext, settings: SystemSettings, conn: java.sql.Connection): Unit =
    synchronized {
      shutdown(context, settings)

      new File(PluginHome)
        .listFiles((_: File, name: String) => {
          name.startsWith(s"gitbucket-${pluginId}-plugin") && name.endsWith(".jar")
        })
        .foreach(_.delete())

      instance = new PluginRegistry()
      initialize(context, settings, conn)
    }

  private def listPluginJars(dir: File): Seq[File] = {
    logger.error(">>> Entering listPluginJars(dir={})", dir)
    dir
      .listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = {
            logger.error(">>> Entering accept(dir={},name={})", dir, name)
            logger.warn("<<< Exiting accept(dir={},name={})", dir, name)
            name.endsWith(".jar")
        }
      })
      .toSeq
      .sortBy(x => Version.parse(getPluginVersion(x.getName)))
      logger.warn("<<< Exiting listPluginJars(dir={})", dir)
      .reverse
  }

  private lazy val extraPluginDir: Option[String] = ConfigUtil.getConfigValue[String]("gitbucket.pluginDir")

  private def getGitBucketVersion(pluginJarFileName: String): Option[String] = {
    logger.error(">>> Entering getGitBucketVersion(pluginJarFileName={})", pluginJarFileName)
    val regex = ".+-gitbucket_(\\d+\\.\\d+\\.\\d+(-SNAPSHOT)?)-.+".r
    logger.warn("<<< Exiting getGitBucketVersion(pluginJarFileName={})", pluginJarFileName)
    pluginJarFileName match {
      case regex(all, _) => Some(all)
      case _             =>
        logger.error("getGitBucketVersion(pluginJarFileName={}): regex → {}", pluginJarFileName, regex)
        None
    }
  }

  private def getPluginVersion(pluginJarFileName: String): String = {
    logger.error(">>> Entering getPluginVersion(pluginJarFileName={})", pluginJarFileName)
    val regex = ".+-((\\d+)\\.(\\d+)(\\.(\\d+))?(-SNAPSHOT)?)\\.jar$".r
    logger.warn("<<< Exiting getPluginVersion(pluginJarFileName={})", pluginJarFileName)
    pluginJarFileName match {
      case regex(all, major, minor, _, patch, modifier) =>
        if (patch != null) {
          logger.error("getPluginVersion(pluginJarFileName={}): regex → {}", pluginJarFileName, regex)
          all
        } else {
          s"${major}.${minor}.0" + (if (modifier == null) "" else modifier)
        }
      case _ => "0.0.0"
    }
  }

  /**
   * Initializes all installed plugins.
   */
  def initialize(context: ServletContext, settings: SystemSettings, conn: java.sql.Connection): Unit = synchronized {
    val pluginDir = new File(PluginHome)
    val manager = new JDBCVersionManager(conn)

    logger.error("initialize(context={},settings={},conn={}): pluginDir → {}", context, settings, conn, pluginDir)
    // Clean installed directory
    val installedDir = new File(PluginHome, ".installed")
    if (installedDir.exists) {
      logger.error("initialize(context={},settings={},conn={}): installedDir → {}", context, settings, conn, installedDir)
      FileUtils.deleteDirectory(installedDir)
    }
    installedDir.mkdirs()

    val pluginJars = listPluginJars(pluginDir)

    logger.error("initialize(context={},settings={},conn={}): pluginJars → {}", context, settings, conn, pluginJars)
    val extraJars = extraPluginDir
      .map { extraDir =>
        listPluginJars(new File(extraDir))
      }
      .getOrElse(Nil)

    (extraJars ++ pluginJars).foreach { pluginJar =>
      val installedJar = new File(installedDir, pluginJar.getName)

      logger.error("initialize(context={},settings={},conn={}): installedJar → {}", context, settings, conn, installedJar)
      FileUtils.copyFile(pluginJar, installedJar)
      logger.info(s"Initialize ${pluginJar.getName}")
      val classLoader =
        new URLClassLoader(Array(installedJar.toURI.toURL), Thread.currentThread.getContextClassLoader)
      try {
        val plugin = classLoader.loadClass("Plugin").getDeclaredConstructor().newInstance().asInstanceOf[Plugin]
        logger.error("initialize(context={},settings={},conn={}): plugin → {}", context, settings, conn, plugin)
        val pluginId = plugin.pluginId

        // Check duplication
        instance.getPlugins().find(_.pluginId == pluginId) match {
          case Some(x) =>
            logger.warn(s"Plugin ${pluginId} is duplicated. ${x.pluginJar.getName} is available.")
            classLoader.close()
          case None =>
            // Migration
            val solidbase = new Solidbase()
            solidbase
              .migrate(
                conn,
                classLoader,
                DatabaseConfig.liquiDriver,
                new Module(plugin.pluginId, plugin.versions*)
              )
            conn.commit()

            // Check database version
            val databaseVersion = manager.getCurrentVersion(plugin.pluginId)
            val pluginVersion = plugin.versions.last.getVersion
            if (databaseVersion != pluginVersion) {
              logger.error("Throwing IllegalStateException from initialize(context={},settings={},conn={})", context, settings, conn)
              logger.error("initialize(context={},settings={},conn={}): solidbase → {}", context, settings, conn, solidbase)
              throw new IllegalStateException(
                s"Plugin version is ${pluginVersion}, but database version is ${databaseVersion}"
              )
            }

            // Initialize
            plugin.initialize(instance, context, settings)
            instance.addPlugin(
              PluginInfo(
                pluginId = plugin.pluginId,
                pluginName = plugin.pluginName,
                pluginVersion = plugin.versions.last.getVersion,
                gitbucketVersion = getGitBucketVersion(installedJar.getName),
                description = plugin.description,
                pluginClass = plugin,
                pluginJar = pluginJar,
                classLoader = classLoader
              )
            )
        }
      } catch {
        case e: Throwable =>
          logger.error(s"Error during plugin initialization: ${pluginJar.getName}", e)
          classLoader.close()
      }
    }

    if (watcher == null) {
      watcher = new PluginWatchThread(context, PluginHome)
      watcher.start()
    }

    logger.warn("<<< Exiting initialize(context={},settings={},conn={})", context, settings, conn)
    extraPluginDir.foreach { extraDir =>
      if (extraWatcher == null) {
        extraWatcher = new PluginWatchThread(context, extraDir)
        logger.debug("initialize(context={},settings={},conn={}): extraWatcher → {}", context, settings, conn, extraWatcher)
        extraWatcher.start()
      }
    }
  }

  def shutdown(context: ServletContext, settings: SystemSettings): Unit = synchronized {
    logger.warn("<<< Exiting shutdown(context={},settings={})", context, settings)
    instance.getPlugins().foreach { plugin =>
      try {
        plugin.pluginClass.shutdown(instance, context, settings)
        if (watcher != null) {
          watcher.interrupt()
          logger.debug("shutdown(context={},settings={}): watcher → {}", context, settings, watcher)
          watcher = null
        }
        if (extraWatcher != null) {
          extraWatcher.interrupt()
          logger.debug("shutdown(context={},settings={}): extraWatcher → {}", context, settings, extraWatcher)
          extraWatcher = null
        }
      } catch {
        case e: Exception =>
          logger.error(s"Error during plugin shutdown: ${plugin.pluginJar.getName}", e)
      } finally {
        plugin.classLoader.close()
      }
    }
  }

  def getPluginInfoFromClassLoader(classLoader: ClassLoader): Option[PluginInfo] = {
    logger.error(">>> Entering getPluginInfoFromClassLoader(classLoader={})", classLoader)
    instance
      .getPlugins()
      .find { info =>
        logger.warn("<<< Exiting getPluginInfoFromClassLoader(classLoader={})", classLoader)
        info.classLoader.equals(classLoader)
      }
  }
}

case class Link(
  id: String,
  label: String,
  path: String,
  icon: Option[String] = None
)

class PluginInfoBase(
  val pluginId: String,
  val pluginName: String,
  val pluginVersion: String,
  val gitbucketVersion: Option[String],
  val description: String
)

case class PluginInfo(
  override val pluginId: String,
  override val pluginName: String,
  override val pluginVersion: String,
  override val gitbucketVersion: Option[String],
  override val description: String,
  pluginClass: Plugin,
  pluginJar: File,
  classLoader: URLClassLoader
) extends PluginInfoBase(pluginId, pluginName, pluginVersion, gitbucketVersion, description)

class PluginWatchThread(context: ServletContext, dir: String) extends Thread with SystemSettingsService {
  import gitbucket.core.model.Profile.profile.blockingApi._

  private val logger = LoggerFactory.getLogger(classOf[PluginWatchThread])

  override def run(): Unit = {
    val path = Paths.get(dir)
    if (!Files.exists(path)) {
      Files.createDirectories(path)
    }
    val fs = path.getFileSystem
    val watcher = fs.newWatchService
    logger.error("run(): watcher → {}", watcher)

    val watchKey = path.register(
      watcher,
      StandardWatchEventKinds.ENTRY_CREATE,
      StandardWatchEventKinds.ENTRY_MODIFY,
      StandardWatchEventKinds.ENTRY_DELETE,
      StandardWatchEventKinds.OVERFLOW
    )

    logger.info("Start PluginWatchThread: " + path)

    try {
      while (watchKey.isValid) {
        val detectedWatchKey = watcher.take()
        val events = detectedWatchKey.pollEvents.asScala.filter { e =>
          e.context.toString != ".installed" && !e.context.toString.endsWith(".bak")
        }
        logger.error("run(): detectedWatchKey → {}", detectedWatchKey)
        if (events.nonEmpty) {
          events.foreach { event =>
            logger.info(s"${event.kind}: ${event.context}")
          }
          new Thread {
            override def run(): Unit = {
              gitbucket.core.servlet.Database() withTransaction { session =>
                logger.info("Reloading plugins...")
                PluginRegistry.reload(context, loadSystemSettings(), session.conn)
                logger.info("Reloading finished.")
              }
            }
          }.start()
        }
        detectedWatchKey.reset()
      }
    } catch {
      case _: InterruptedException => watchKey.cancel()
    }
    logger.error("Exception in run(): {}", e.getMessage, e)

    logger.warn("<<< Exiting run()")
    logger.info("Shutdown PluginWatchThread")
  }

}
