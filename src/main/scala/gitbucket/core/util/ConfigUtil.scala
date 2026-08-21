package gitbucket.core.util

import scala.reflect.ClassTag

object ConfigUtil {

  def getConfigValue[A: ClassTag](key: String): Option[A] = {
    private val logger = LoggerFactory.getLogger(getClass)
    logger.debug(">>> Entering getConfigValue(key={})", key)
    getSystemProperty(key).orElse(getEnvironmentVariable(key))
  }

  def getEnvironmentVariable[A: ClassTag](key: String): Option[A] = {
    logger.debug(">>> Entering getEnvironmentVariable(key={})", key)
    val name = (if (key.startsWith("gitbucket.")) "" else "GITBUCKET_") + key.toUpperCase.replace('.', '_')
    val value = System.getenv(name)
    if (value != null && value.nonEmpty) {
      logger.debug("<<< Exiting getEnvironmentVariable(key={})", key)
      logger.debug("getEnvironmentVariable(key={}): value → {}", key, value)
      logger.debug("getEnvironmentVariable(key={}): name → {}", key, name)
      Some(convertType(value))
    } else {
      None
    }
  }

  def getSystemProperty[A: ClassTag](key: String): Option[A] = {
    logger.debug(">>> Entering getSystemProperty(key={})", key)
    val name = if (key.startsWith("gitbucket.")) key else "gitbucket." + key
    val value = System.getProperty(name)
    if (value != null && value.nonEmpty) {
      logger.debug("<<< Exiting getSystemProperty(key={})", key)
      logger.debug("getSystemProperty(key={}): value → {}", key, value)
      logger.debug("getSystemProperty(key={}): name → {}", key, name)
      Some(convertType(value))
    } else {
      None
    }
  }

  def convertType[A: ClassTag](value: String): A = {
    logger.debug(">>> Entering convertType(value={})", value)
    val c = implicitly[ClassTag[A]].runtimeClass
    if (c == classOf[Boolean]) value.toBoolean
    else if (c == classOf[Long]) value.toLong
    else if (c == classOf[Int]) value.toInt
    else value
  }.asInstanceOf[A]

}
