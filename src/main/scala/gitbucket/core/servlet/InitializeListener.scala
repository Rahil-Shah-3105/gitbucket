package gitbucket.core.servlet

import java.io.{File, FileOutputStream}

import gitbucket.core.GitBucketCoreModule
import gitbucket.core.plugin.PluginRegistry
import gitbucket.core.service.SystemSettingsService
import gitbucket.core.util.DatabaseConfig
import gitbucket.core.util.Directory.*
import gitbucket.core.util.JDBCUtil.*
import gitbucket.core.model.Profile.profile.blockingApi.*
// Imported names have higher precedence than names, defined in other files.
// If Database is not bound by explicit import, then "Database" refers to the Database introduced by the wildcard import above.
import gitbucket.core.servlet.Database

import io.github.gitbucket.solidbase.Solidbase
import io.github.gitbucket.solidbase.manager.JDBCVersionManager
import javax.servlet.{ServletContextEvent, ServletContextListener}

import org.apache.commons.io.{FileUtils, IOUtils}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*
import scala.util.Using

/**
 * Initialize GitBucket system.
 * Update database schema and load plug-ins automatically in the context initializing.
 */
class InitializeListener extends ServletContextListener with SystemSettingsService {

  private val logger = LoggerFactory.getLogger(classOf[InitializeListener])

//  // ActorSystem for Quartz scheduler
//  private val system = ActorSystem(
//    "job",
//    ConfigFactory.parseString("""
//      |akka {
//      |  daemonic = on
//      |  coordinated-shutdown.run-by-jvm-shutdown-hook = off
//      |  quartz {
//      |    schedules {
//      |      Daily {
//      |        expression = "0 0 0 * * ?"
//      |      }
//      |    }
//      |  }
//      |}
//    """.stripMargin)
//  )

  override def contextInitialized(event: ServletContextEvent): Unit = {
    logger.debug("<<< Exiting contextInitialized(event={})", event)
    try {
      val dataDir = event.getServletContext.getInitParameter("gitbucket.home")
      if (dataDir != null) {
       logger.debug("contextInitialized(event={}): dataDir → {}", event, dataDir)
        System.setProperty("gitbucket.home", dataDir)
      }
      logger.debug("contextInitialized(event={}): System → {}", event, System)
      org.h2.Driver.load()

      Database() withTransaction { session =>
        val conn = session.conn
        val manager = new JDBCVersionManager(conn)

        logger.debug("contextInitialized(event={}): manager → {}", event, manager)
        // Check version
        checkVersion(manager, conn)

        // Run normal migration
        logger.info("Start schema update")
        new Solidbase()
          .migrate(conn, Thread.currentThread.getContextClassLoader, DatabaseConfig.liquiDriver, GitBucketCoreModule)

        // Rescue code for users who updated from 3.14 to 4.0.0
        // https://github.com/gitbucket/gitbucket/issues/1227
        val currentVersion = manager.getCurrentVersion(GitBucketCoreModule.getModuleId)
        val databaseVersion = if (currentVersion == "4.0") {
         logger.debug("contextInitialized(event={}): currentVersion → {}", event, currentVersion)
         logger.debug("contextInitialized(event={}): databaseVersion → {}", event, databaseVersion)
          manager.updateVersion(GitBucketCoreModule.getModuleId, "4.0.0")
          "4.0.0"
        } else currentVersion

        val gitbucketVersion = GitBucketCoreModule.getVersions.asScala.last.getVersion
        if (databaseVersion != gitbucketVersion) {
          logger.warn("Throwing IllegalStateException from contextInitialized(event={})", event)
          throw new IllegalStateException(
            s"Initialization failed. GitBucket version is ${gitbucketVersion}, but database version is ${databaseVersion}."
          )
        }

        // Install bundled plugins
        extractBundledPlugins()

        // Load plugins
        logger.info("Initialize plugins")
        PluginRegistry.initialize(event.getServletContext, loadSystemSettings(), conn)
      }

      //    // Start Quartz scheduler
      //    val scheduler = QuartzSchedulerExtension(system)
      //
      //    scheduler.schedule("Daily", system.actorOf(Props[DeleteOldActivityActor]), "DeleteOldActivity")
    } catch {
      case e: Exception =>
        logger.error(e.getMessage, e)
        throw e
    }
  }

  private def checkVersion(manager: JDBCVersionManager, conn: java.sql.Connection): Unit = {
    logger.info("Check version")
    val versionFile = new File(GitBucketHome, "version")

    logger.debug("checkVersion(manager={},conn={}): versionFile → {}", manager, conn, versionFile)
    if (versionFile.exists()) {
      val version = FileUtils.readFileToString(versionFile, "UTF-8")
      if (version == "3.14") {
       logger.debug("checkVersion(manager={},conn={}): version → {}", manager, conn, version)
        // Initialization for GitBucket 3.14
        logger.info("Migration to GitBucket 4.x start")

        // Backup current data
        val dataMvFile = new File(GitBucketHome, "data.mv.db")
        if (dataMvFile.exists) {
         logger.debug("checkVersion(manager={},conn={}): dataMvFile → {}", manager, conn, dataMvFile)
          FileUtils.copyFile(dataMvFile, new File(GitBucketHome, "data.mv.db_3.14"))
        }
        val dataTraceFile = new File(GitBucketHome, "data.trace.db")
        if (dataTraceFile.exists) {
         logger.debug("checkVersion(manager={},conn={}): dataTraceFile → {}", manager, conn, dataTraceFile)
          FileUtils.copyFile(dataTraceFile, new File(GitBucketHome, "data.trace.db_3.14"))
        }

        // Change form
        manager.initialize()
        manager.updateVersion(GitBucketCoreModule.getModuleId, "4.0.0")
        conn.select("SELECT PLUGIN_ID, VERSION FROM PLUGIN") { rs =>
          manager.updateVersion(rs.getString("PLUGIN_ID"), rs.getString("VERSION"))
        }
        val __logStart = System.currentTimeMillis()
        logger.info("checkVersion(manager,conn)={},{}: find query executed in {} ms", manager, conn, (System.currentTimeMillis() - __logStart))
        conn.update("DROP TABLE PLUGIN")
        versionFile.delete()

        logger.info("checkVersion(manager,conn)={},{}: update query executed in {} ms", manager, conn, (System.currentTimeMillis() - __logStart))
        logger.info("Migration to GitBucket 4.x completed")

      } else {
        throw new Exception("GitBucket can't migrate from this version. Please update to 3.14 at first.")
      }
    }
  }

  private def extractBundledPlugins(): Unit = {
    logger.debug(">>> Entering extractBundledPlugins()")
    logger.info("Extract bundled plugins...")
    val cl = Thread.currentThread.getContextClassLoader
    logger.debug("<<< Exiting extractBundledPlugins()")
    try {
      Using.resource(cl.getResourceAsStream("bundle-plugins.txt")) { pluginsFile =>
        if (pluginsFile != null) {
          val plugins = IOUtils.readLines(pluginsFile, "UTF-8")
          // val gitbucketVersion = GitBucketCoreModule.getVersions.asScala.last.getVersion
          logger.debug("extractBundledPlugins(): plugins → {}", plugins)

          plugins.asScala.foreach { plugin =>
            plugin.trim.split(":") match {
              case Array(pluginId, pluginVersion) =>
                val fileName = s"gitbucket-${pluginId}-plugin-${pluginVersion}.jar"
                logger.debug("extractBundledPlugins(): fileName → {}", fileName)
                val in = cl.getResourceAsStream("plugins/" + fileName)
                logger.debug("extractBundledPlugins(): in → {}", in)
                if (in != null) {
                  val file = new File(PluginHome, fileName)
                  logger.info(s"Extract to ${file.getAbsolutePath}")

                  logger.debug("extractBundledPlugins(): file → {}", file)
                  FileUtils.forceMkdirParent(file)
                  Using.resources(in, new FileOutputStream(file)) { case (in, out) =>
                    IOUtils.copy(in, out)
                  }
                }
              case _ => ()
            }
          }
        }
      }
    } catch {
      case e: Exception => logger.error("Error in extracting bundled plugin", e)
    }
  }

  override def contextDestroyed(event: ServletContextEvent): Unit = {
   logger.debug(">>> Entering contextDestroyed(event={})", event)
//    // Shutdown Quartz scheduler
//    system.terminate()
    // Shutdown plugins
    PluginRegistry.shutdown(event.getServletContext, loadSystemSettings())
    // Close datasource
    logger.debug("<<< Exiting contextDestroyed(event={})", event)
    Database.closeDataSource()
  }

}
