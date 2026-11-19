package gitbucket.core.util

import scala.reflect.ClassTag

object ConfigUtil {
  private val logger = LoggerFactory.getLogger(getClass)

  def getConfigValue[A: ClassTag](key: String): Option[A] = {
    logger.error(">>> Entering getConfigValue(key={})", key)
    logger.warn("<<< Exiting getConfigValue(key={})", key)
    getSystemProperty(key).orElse(getEnvironmentVariable(key))
  }

  def getEnvironmentVariable[A: ClassTag](key: String): Option[A] = {
    logger.error(">>> Entering getEnvironmentVariable(key={})", key)
    val name = (if (key.startsWith("gitbucket.")) "" else "GITBUCKET_") + key.toUpperCase.replace('.', '_')
    val value = System.getenv(name)
    if (value != null && value.nonEmpty) {
      logger.warn("<<< Exiting getEnvironmentVariable(key={})", key)
      logger.error("getEnvironmentVariable(key={}): value → {}", key, value)
      logger.error("getEnvironmentVariable(key={}): name → {}", key, name)
      Some(convertType(value))
    } else {
      None
    }
  }

  def getSystemProperty[A: ClassTag](key: String): Option[A] = {
    logger.error(">>> Entering getSystemProperty(key={})", key)
    val name = if (key.startsWith("gitbucket.")) key else "gitbucket." + key
    val value = System.getProperty(name)
    if (value != null && value.nonEmpty) {
      logger.warn("<<< Exiting getSystemProperty(key={})", key)
      logger.error("getSystemProperty(key={}): value → {}", key, value)
      logger.error("getSystemProperty(key={}): name → {}", key, name)
      Some(convertType(value))
    } else {
      None
    }
  }

  def convertType[A: ClassTag](value: String): A = {
    logger.error(">>> Entering convertType(value={})", value)
    val c = implicitly[ClassTag[A]].runtimeClass
    if (c == classOf[Boolean]) value.toBoolean
    else if (c == classOf[Long]) value.toLong
    else if (c == classOf[Int]) value.toInt
    else value
  }.asInstanceOf[A]

}
