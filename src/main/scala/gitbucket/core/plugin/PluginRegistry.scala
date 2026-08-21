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

  def addPlugin(pluginInfo: PluginInfo): Unit = plugins.add(pluginInfo)

  def getPlugins(): List[PluginInfo] = plugins.asScala.toList

  def addImage(id: String, bytes: Array[Byte]): Unit = {
    logger.debug(">>> Entering addImage(id={},bytes={})", id, bytes)
    val encoded = Base64.getEncoder.encodeToString(bytes)
    logger.debug("<<< Exiting addImage(id={},bytes={})", id, bytes)
    logger.debug("addImage(id={},bytes={}): encoded → {}", id, bytes, encoded)
    images.put(id, encoded)
  }

  def getImage(id: String): String = images.get(id)
  logger.debug(">>> Entering getImage(id={})", id)

  def addController(path: String, controller: ControllerBase): Unit = controllers.add((controller, path))
  logger.debug(">>> Entering addController(path={},controller={})", path, controller)

  def getControllers(): Seq[(ControllerBase, String)] = controllers.asScala.toSeq
  logger.debug(">>> Entering getControllers()")

  def addAnonymousAccessiblePath(path: String): Unit = anonymousAccessiblePaths.add(path)
  logger.debug(">>> Entering addAnonymousAccessiblePath(path={})", path)

  def getAnonymousAccessiblePaths(): Seq[String] = anonymousAccessiblePaths.asScala.toSeq
  logger.debug(">>> Entering getAnonymousAccessiblePaths()")

  def addJavaScript(path: String, script: String): Unit =
    logger.debug(">>> Entering addJavaScript(path={},script={})", path, script)
    logger.debug("<<< Exiting addJavaScript(path={},script={})", path, script)
    javaScripts.add((path, script)) // javaScripts += ((path, script))

  def getJavaScript(currentPath: String): List[String] =
    logger.debug(">>> Entering getJavaScript(currentPath={})", currentPath)
    logger.debug("<<< Exiting getJavaScript(currentPath={})", currentPath)
    javaScripts.asScala.filter(x => currentPath.matches(x._1)).toList.map(_._2)

  def addRenderer(extension: String, renderer: Renderer): Unit = renderers.put(extension, renderer)
  logger.debug(">>> Entering addRenderer(extension={},renderer={})", extension, renderer)

  def getRenderer(extension: String): Renderer = renderers.asScala.getOrElse(extension, DefaultRenderer)
  logger.debug(">>> Entering getRenderer(extension={})", extension)

  def renderableExtensions: Seq[String] = renderers.keys.asScala.toSeq
  logger.debug("<<< Exiting getRenderer(extension={})", extension)

  def addRepositoryRouting(routing: GitRepositoryRouting): Unit = repositoryRoutings.add(routing)

  def getRepositoryRoutings(): Seq[GitRepositoryRouting] = repositoryRoutings.asScala.toSeq

  def getRepositoryRouting(repositoryPath: String): Option[GitRepositoryRouting] = {
    logger.debug(">>> Entering getRepositoryRouting(repositoryPath={})", repositoryPath)
    PluginRegistry().getRepositoryRoutings().find {
      logger.debug("<<< Exiting getRepositoryRouting(repositoryPath={})", repositoryPath)
      case GitRepositoryRouting(urlPath, _, _) => {
        repositoryPath.matches("/" + urlPath + "(/.*)?")
      }
    }
  }

  def addAccountHook(accountHook: AccountHook): Unit = accountHooks.add(accountHook)
  logger.debug(">>> Entering addAccountHook(accountHook={})", accountHook)

  def getAccountHooks: Seq[AccountHook] = accountHooks.asScala.toSeq
  logger.debug("<<< Exiting addAccountHook(accountHook={})", accountHook)

  def addReceiveHook(commitHook: ReceiveHook): Unit = receiveHooks.add(commitHook)
  logger.debug(">>> Entering addReceiveHook(commitHook={})", commitHook)

  def getReceiveHooks: Seq[ReceiveHook] = receiveHooks.asScala.toSeq
  logger.debug("<<< Exiting addReceiveHook(commitHook={})", commitHook)

  def addRepositoryHook(repositoryHook: RepositoryHook): Unit = repositoryHooks.add(repositoryHook)
  logger.debug(">>> Entering addRepositoryHook(repositoryHook={})", repositoryHook)

  def getRepositoryHooks: Seq[RepositoryHook] = repositoryHooks.asScala.toSeq
  logger.debug("<<< Exiting addRepositoryHook(repositoryHook={})", repositoryHook)

  def addIssueHook(issueHook: IssueHook): Unit = issueHooks.add(issueHook)
  logger.debug(">>> Entering addIssueHook(issueHook={})", issueHook)

  def getIssueHooks: Seq[IssueHook] = issueHooks.asScala.toSeq
  logger.debug("<<< Exiting addIssueHook(issueHook={})", issueHook)

  def addPullRequestHook(pullRequestHook: PullRequestHook): Unit = pullRequestHooks.add(pullRequestHook)
  logger.debug(">>> Entering addPullRequestHook(pullRequestHook={})", pullRequestHook)

  def getPullRequestHooks: Seq[PullRequestHook] = pullRequestHooks.asScala.toSeq
  logger.debug("<<< Exiting addPullRequestHook(pullRequestHook={})", pullRequestHook)

  def addRepositoryHeader(repositoryHeader: (RepositoryInfo, Context) => Option[Html]): Unit =
    logger.debug(">>> Entering addRepositoryHeader(repositoryHeader={})", repositoryHeader)
    logger.debug("<<< Exiting addRepositoryHeader(repositoryHeader={})", repositoryHeader)
    repositoryHeaders.add(repositoryHeader)

  def getRepositoryHeaders: Seq[(RepositoryInfo, Context) => Option[Html]] = repositoryHeaders.asScala.toSeq

  def addGlobalMenu(globalMenu: (Context) => Option[Link]): Unit = globalMenus.add(globalMenu)
  logger.debug(">>> Entering addGlobalMenu(globalMenu={})", globalMenu)

  def getGlobalMenus: Seq[(Context) => Option[Link]] = globalMenus.asScala.toSeq
  logger.debug("<<< Exiting addGlobalMenu(globalMenu={})", globalMenu)

  def addRepositoryMenu(repositoryMenu: (RepositoryInfo, Context) => Option[Link]): Unit =
    logger.debug(">>> Entering addRepositoryMenu(repositoryMenu={})", repositoryMenu)
    logger.debug("<<< Exiting addRepositoryMenu(repositoryMenu={})", repositoryMenu)
    repositoryMenus.add(repositoryMenu)

  def getRepositoryMenus: Seq[(RepositoryInfo, Context) => Option[Link]] = repositoryMenus.asScala.toSeq

  def addRepositorySettingTab(repositorySettingTab: (RepositoryInfo, Context) => Option[Link]): Unit =
    logger.debug(">>> Entering addRepositorySettingTab(repositorySettingTab={})", repositorySettingTab)
    logger.debug("<<< Exiting addRepositorySettingTab(repositorySettingTab={})", repositorySettingTab)
    repositorySettingTabs.add(repositorySettingTab)

  def getRepositorySettingTabs: Seq[(RepositoryInfo, Context) => Option[Link]] = repositorySettingTabs.asScala.toSeq

  def addProfileTab(profileTab: (Account, Context) => Option[Link]): Unit = profileTabs.add(profileTab)
  logger.debug(">>> Entering addProfileTab(profileTab={})", profileTab)

  def getProfileTabs: Seq[(Account, Context) => Option[Link]] = profileTabs.asScala.toSeq
  logger.debug("<<< Exiting addProfileTab(profileTab={})", profileTab)

  def addSystemSettingMenu(systemSettingMenu: (Context) => Option[Link]): Unit =
    logger.debug(">>> Entering addSystemSettingMenu(systemSettingMenu={})", systemSettingMenu)
    logger.debug("<<< Exiting addSystemSettingMenu(systemSettingMenu={})", systemSettingMenu)
    systemSettingMenus.add(systemSettingMenu)

  def getSystemSettingMenus: Seq[(Context) => Option[Link]] = systemSettingMenus.asScala.toSeq

  def addAccountSettingMenu(accountSettingMenu: (Context) => Option[Link]): Unit =
    logger.debug(">>> Entering addAccountSettingMenu(accountSettingMenu={})", accountSettingMenu)
    logger.debug("<<< Exiting addAccountSettingMenu(accountSettingMenu={})", accountSettingMenu)
    accountSettingMenus.add(accountSettingMenu)

  def getAccountSettingMenus: Seq[(Context) => Option[Link]] = accountSettingMenus.asScala.toSeq

  def addDashboardTab(dashboardTab: (Context) => Option[Link]): Unit = dashboardTabs.add(dashboardTab)
  logger.debug(">>> Entering addDashboardTab(dashboardTab={})", dashboardTab)

  def getDashboardTabs: Seq[(Context) => Option[Link]] = dashboardTabs.asScala.toSeq
  logger.debug("<<< Exiting addDashboardTab(dashboardTab={})", dashboardTab)

  def addIssueSidebar(issueSidebar: (Issue, RepositoryInfo, Context) => Option[Html]): Unit =
    logger.debug(">>> Entering addIssueSidebar(issueSidebar={})", issueSidebar)
    logger.debug("<<< Exiting addIssueSidebar(issueSidebar={})", issueSidebar)
    issueSidebars.add(issueSidebar)

  def getIssueSidebars: Seq[(Issue, RepositoryInfo, Context) => Option[Html]] = issueSidebars.asScala.toSeq

  def addAssetsMapping(assetsMapping: (String, String, ClassLoader)): Unit = assetsMappings.add(assetsMapping)
  logger.debug(">>> Entering addAssetsMapping(assetsMapping=<redacted>)")

  def getAssetsMappings: Seq[(String, String, ClassLoader)] = assetsMappings.asScala.toSeq
  logger.debug("<<< Exiting addAssetsMapping(assetsMapping=<redacted>)")

  def addTextDecorator(textDecorator: TextDecorator): Unit = textDecorators.add(textDecorator)
  logger.debug(">>> Entering addTextDecorator(textDecorator={})", textDecorator)

  def getTextDecorators: Seq[TextDecorator] = textDecorators.asScala.toSeq
  logger.debug("<<< Exiting addTextDecorator(textDecorator={})", textDecorator)

  def addSuggestionProvider(suggestionProvider: SuggestionProvider): Unit = suggestionProviders.add(suggestionProvider)
  logger.debug(">>> Entering addSuggestionProvider(suggestionProvider={})", suggestionProvider)

  def getSuggestionProviders: Seq[SuggestionProvider] = suggestionProviders.asScala.toSeq
  logger.debug("<<< Exiting addSuggestionProvider(suggestionProvider={})", suggestionProvider)

  def addSshCommandProvider(sshCommandProvider: PartialFunction[String, ChannelSession => Command]): Unit =
    logger.debug(">>> Entering addSshCommandProvider(sshCommandProvider={})", sshCommandProvider)
    logger.debug("addSshCommandProvider(sshCommandProvider={}): logger → {}", sshCommandProvider, logger)
    logger.debug("addSshCommandProvider(sshCommandProvider={}): instance → {}", sshCommandProvider, instance)
    logger.debug("<<< Exiting addSshCommandProvider(sshCommandProvider={})", sshCommandProvider)
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
  def apply(): PluginRegistry = instance

  logger.debug(">>> Entering apply()")
  /**
   * Reload all plugins.
   */
  def reload(context: ServletContext, settings: SystemSettings, conn: java.sql.Connection): Unit = synchronized {
    logger.debug(">>> Entering reload(context={},settings={},conn={})", context, settings, conn)
    shutdown(context, settings)
    instance = new PluginRegistry()
    logger.debug("<<< Exiting reload(context={},settings={},conn={})", context, settings, conn)
    logger.debug("reload(context={},settings={},conn={}): instance → {}", context, settings, conn, instance)
    initialize(context, settings, conn)
  }

  /**
   * Uninstall a specified plugin.
   */
  def uninstall(pluginId: String, context: ServletContext, settings: SystemSettings, conn: java.sql.Connection): Unit =
    synchronized {
      logger.debug(">>> Entering uninstall(pluginId={},context={},settings={},conn={})", pluginId, context, settings, conn)
      shutdown(context, settings)

      new File(PluginHome)
        .listFiles((_: File, name: String) => {
          name.startsWith(s"gitbucket-${pluginId}-plugin") && name.endsWith(".jar")
        })
        .foreach(_.delete())

      instance = new PluginRegistry()
      logger.debug("<<< Exiting uninstall(pluginId={},context={},settings={},conn={})", pluginId, context, settings, conn)
      logger.debug("uninstall(pluginId={},context={},settings={},conn={}): instance → {}", pluginId, context, settings, conn, instance)
      initialize(context, settings, conn)
    }

  private def listPluginJars(dir: File): Seq[File] = {
    logger.debug(">>> Entering listPluginJars(dir={})", dir)
    dir
      .listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = name.endsWith(".jar")
      })
      .toSeq
      logger.debug(">>> Entering accept(dir={},name={})", dir, name)
      .sortBy(x => Version.parse(getPluginVersion(x.getName)))
      logger.debug("<<< Exiting accept(dir={},name={})", dir, name)
      .reverse
  }

  logger.debug("accept(dir={},name={}): extraPluginDir → {}", dir, name, extraPluginDir)
  private lazy val extraPluginDir: Option[String] = ConfigUtil.getConfigValue[String]("gitbucket.pluginDir")

  private def getGitBucketVersion(pluginJarFileName: String): Option[String] = {
    logger.debug(">>> Entering getGitBucketVersion(pluginJarFileName={})", pluginJarFileName)
    val regex = ".+-gitbucket_(\\d+\\.\\d+\\.\\d+(-SNAPSHOT)?)-.+".r
    logger.debug("<<< Exiting getGitBucketVersion(pluginJarFileName={})", pluginJarFileName)
    pluginJarFileName match {
      case regex(all, _) => Some(all)
      case _             =>
        logger.debug("getGitBucketVersion(pluginJarFileName={}): regex → {}", pluginJarFileName, regex)
        None
    }
  }

  private def getPluginVersion(pluginJarFileName: String): String = {
    logger.debug(">>> Entering getPluginVersion(pluginJarFileName={})", pluginJarFileName)
    val regex = ".+-((\\d+)\\.(\\d+)(\\.(\\d+))?(-SNAPSHOT)?)\\.jar$".r
    logger.debug("<<< Exiting getPluginVersion(pluginJarFileName={})", pluginJarFileName)
    pluginJarFileName match {
      case regex(all, major, minor, _, patch, modifier) =>
        if (patch != null) {
         logger.debug("getPluginVersion(pluginJarFileName={}): regex → {}", pluginJarFileName, regex)
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
    logger.debug(">>> Entering initialize(context={},settings={},conn={})", context, settings, conn)
    val pluginDir = new File(PluginHome)
    val manager = new JDBCVersionManager(conn)

    logger.debug("initialize(context={},settings={},conn={}): manager → {}", context, settings, conn, manager)
    logger.debug("initialize(context={},settings={},conn={}): pluginDir → {}", context, settings, conn, pluginDir)
    // Clean installed directory
    val installedDir = new File(PluginHome, ".installed")
    if (installedDir.exists) {
     logger.debug("initialize(context={},settings={},conn={}): installedDir → {}", context, settings, conn, installedDir)
      FileUtils.deleteDirectory(installedDir)
    }
    installedDir.mkdirs()

    val pluginJars = listPluginJars(pluginDir)

    logger.debug("initialize(context={},settings={},conn={}): pluginJars → {}", context, settings, conn, pluginJars)
    val extraJars = extraPluginDir
      .map { extraDir =>
        listPluginJars(new File(extraDir))
      }
      .getOrElse(Nil)

    (extraJars ++ pluginJars).foreach { pluginJar =>
      val installedJar = new File(installedDir, pluginJar.getName)

      logger.debug("initialize(context={},settings={},conn={}): installedJar → {}", context, settings, conn, installedJar)
      FileUtils.copyFile(pluginJar, installedJar)
      logger.info(s"Initialize ${pluginJar.getName}")
      val classLoader =
        new URLClassLoader(Array(installedJar.toURI.toURL), Thread.currentThread.getContextClassLoader)
      try {
        val plugin = classLoader.loadClass("Plugin").getDeclaredConstructor().newInstance().asInstanceOf[Plugin]
        logger.debug("initialize(context={},settings={},conn={}): plugin → {}", context, settings, conn, plugin)
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
            logger.debug("initialize(context={},settings={},conn={}): solidbase → {}", context, settings, conn, solidbase)
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
            logger.debug("initialize(context={},settings={},conn={}): databaseVersion → {}", context, settings, conn, databaseVersion)
            if (databaseVersion != pluginVersion) {
              logger.warn("Throwing IllegalStateException from initialize(context={},settings={},conn={})", context, settings, conn)
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
        logger.debug("initialize(context={},settings={},conn={}): pluginName → {}", context, settings, conn, pluginName)
        logger.debug("initialize(context={},settings={},conn={}): gitbucketVersion → {}", context, settings, conn, gitbucketVersion)
        logger.debug("initialize(context={},settings={},conn={}): description → {}", context, settings, conn, description)
        logger.debug("initialize(context={},settings={},conn={}): pluginClass → {}", context, settings, conn, pluginClass)
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
    logger.debug("initialize(context={},settings={},conn={}): watcher → {}", context, settings, conn, watcher)

    logger.debug("<<< Exiting initialize(context={},settings={},conn={})", context, settings, conn)
    extraPluginDir.foreach { extraDir =>
      if (extraWatcher == null) {
        extraWatcher = new PluginWatchThread(context, extraDir)
        extraWatcher.start()
      logger.debug("initialize(context={},settings={},conn={}): extraWatcher → {}", context, settings, conn, extraWatcher)
      }
    }
  }

  def shutdown(context: ServletContext, settings: SystemSettings): Unit = synchronized {
    logger.debug(">>> Entering shutdown(context={},settings={})", context, settings)
    instance.getPlugins().foreach { plugin =>
      try {
      logger.debug("<<< Exiting shutdown(context={},settings={})", context, settings)
        plugin.pluginClass.shutdown(instance, context, settings)
        if (watcher != null) {
          watcher.interrupt()
          watcher = null
          logger.debug("shutdown(context={},settings={}): watcher → {}", context, settings, watcher)
        }
        if (extraWatcher != null) {
          extraWatcher.interrupt()
          extraWatcher = null
          logger.debug("shutdown(context={},settings={}): extraWatcher → {}", context, settings, extraWatcher)
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
    logger.debug(">>> Entering getPluginInfoFromClassLoader(classLoader={})", classLoader)
    instance
      .getPlugins()
      .find { info =>
        info.classLoader.equals(classLoader)
      logger.debug("<<< Exiting getPluginInfoFromClassLoader(classLoader={})", classLoader)
      }
  logger.debug("<<< Exiting apply()")
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
     logger.debug("run(): path → {}", path)
      Files.createDirectories(path)
    }
    val fs = path.getFileSystem
    val watcher = fs.newWatchService
    logger.debug("run(): watcher → {}", watcher)

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
        logger.debug("run(): detectedWatchKey → {}", detectedWatchKey)
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

    logger.debug("<<< Exiting run()")
    logger.info("Shutdown PluginWatchThread")
  }

}
