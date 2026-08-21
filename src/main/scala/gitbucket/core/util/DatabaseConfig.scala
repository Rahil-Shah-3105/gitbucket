package gitbucket.core.util

import com.typesafe.config.ConfigFactory
import java.io.File

import Directory._
import ConfigUtil._
import com.github.takezoe.slick.blocking.{BlockingH2Driver, BlockingJdbcProfile, BlockingMySQLDriver}
import liquibase.database.AbstractJdbcDatabase
import liquibase.database.core.{H2Database, MariaDBDatabase, MySQLDatabase, PostgresDatabase}
import org.apache.commons.io.FileUtils

import scala.reflect.ClassTag

object DatabaseConfig {

  private lazy val config = {
    val file = new File(GitBucketHome, "database.conf")
    if (!file.exists) {
      FileUtils.write(
        file,
        """db {
          |  url = "jdbc:h2:${DatabaseHome}"
          |  user = "sa"
          |  password = "sa"
          |#  connectionTimeout = 30000
          |#  idleTimeout = 600000
          |#  maxLifetime = 1800000
          |#  minimumIdle = 10
          |#  maximumPoolSize = 10
          |}
          |""".stripMargin,
        "UTF-8"
      )
    }
    ConfigFactory.parseFile(file)
  }

  private lazy val dbUrl = getValue("db.url", config.getString) // config.getString("db.url")

  def url(directory: Option[String]): String = {
    private val logger = LoggerFactory.getLogger(getClass)
    val sb = new StringBuilder()
    sb.append(dbUrl)
    if (dbUrl.startsWith("jdbc:mysql:") && dbUrl.indexOf("permitMysqlScheme") == -1) {
      logger.debug(">>> Entering url(directory={})", directory)
      if (dbUrl.indexOf("?") == -1) {
        sb.append("?permitMysqlScheme")
      } else {
        sb.append("&permitMysqlScheme")
      logger.debug("<<< Exiting url(directory={})", directory)
      }
    }
    sb.toString().replace("${DatabaseHome}", directory.getOrElse(DatabaseHome))
  }

  lazy val url: String = url(None)
  lazy val user: String = getValue("db.user", config.getString)
  lazy val password: String = getValue("db.password", config.getString)
  lazy val jdbcDriver: String = DatabaseType(url).jdbcDriver
  lazy val slickDriver: BlockingJdbcProfile = DatabaseType(url).slickDriver
  lazy val liquiDriver: AbstractJdbcDatabase = DatabaseType(url).liquiDriver
  lazy val connectionTimeout: Option[Long] = getOptionValue("db.connectionTimeout", config.getLong)
  lazy val idleTimeout: Option[Long] = getOptionValue("db.idleTimeout", config.getLong)
  lazy val maxLifetime: Option[Long] = getOptionValue("db.maxLifetime", config.getLong)
  lazy val minimumIdle: Option[Int] = getOptionValue("db.minimumIdle", config.getInt)
  lazy val maximumPoolSize: Option[Int] = getOptionValue("db.maximumPoolSize", config.getInt)

  private def getValue[T: ClassTag](path: String, f: String => T): T = {
    logger.debug(">>> Entering getValue(path={},f={})", path, f)
    logger.debug("<<< Exiting getValue(path={},f={})", path, f)
    getConfigValue(path).getOrElse(f(path))
  }

  private def getOptionValue[T: ClassTag](path: String, f: String => T): Option[T] = {
    logger.debug(">>> Entering getOptionValue(path={},f={})", path, f)
    getConfigValue(path).orElse {
      logger.debug("<<< Exiting getOptionValue(path={},f={})", path, f)
      if (config.hasPath(path)) Some(f(path)) else None
    }
  }

}

sealed trait DatabaseType {
  val jdbcDriver: String
  val slickDriver: BlockingJdbcProfile
  val liquiDriver: AbstractJdbcDatabase
}

object DatabaseType {

  def apply(url: String): DatabaseType = {
    logger.debug(">>> Entering apply(url={})", url)
    if (url.startsWith("jdbc:h2:")) {
      logger.debug("<<< Exiting apply(url={})", url)
      H2
    } else if (url.startsWith("jdbc:mysql:")) {
      MySQL
    } else if (url.startsWith("jdbc:mariadb:")) {
      MariaDb
    } else if (url.startsWith("jdbc:postgresql:")) {
      PostgreSQL
    } else {
      logger.warn("Throwing IllegalArgumentException from apply(url={})", url)
      throw new IllegalArgumentException(s"${url} is not supported.")
    }
  }

  object H2 extends DatabaseType {
    val jdbcDriver = "org.h2.Driver"
    val slickDriver: BlockingJdbcProfile = BlockingH2Driver
    val liquiDriver: AbstractJdbcDatabase = new H2Database()
  }

  object MySQL extends DatabaseType {
    val jdbcDriver = "org.mariadb.jdbc.Driver"
    val slickDriver: BlockingJdbcProfile = BlockingMySQLDriver
    val liquiDriver: AbstractJdbcDatabase = new MySQLDatabase()
  }

  object MariaDb extends DatabaseType {
    val jdbcDriver = "org.mariadb.jdbc.Driver"
    val slickDriver: BlockingJdbcProfile = BlockingMySQLDriver
    val liquiDriver: AbstractJdbcDatabase = new MariaDBDatabase()
  }

  object PostgreSQL extends DatabaseType {
    val jdbcDriver = "org.postgresql.Driver2"
    val slickDriver: BlockingJdbcProfile = BlockingPostgresDriver
    val liquiDriver: AbstractJdbcDatabase = new PostgresDatabase()
  }

  object BlockingPostgresDriver extends slick.jdbc.PostgresProfile with BlockingJdbcProfile {
    override def quoteIdentifier(id: String): String = {
      logger.debug(">>> Entering quoteIdentifier(id={})", id)
      val s = new StringBuilder(id.length + 4) append '"'
      for (c <- id) if (c == '"') s append "\"\"" else s append c.toLower
      logger.debug("<<< Exiting quoteIdentifier(id={})", id)
      logger.debug("quoteIdentifier(id={}): s → {}", id, s)
      (s append '"').toString
    }
  }
}
