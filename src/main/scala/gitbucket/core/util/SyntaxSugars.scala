package gitbucket.core.util

/**
 * Provides control facilities.
 */
object SyntaxSugars {
  private val logger = LoggerFactory.getLogger(getClass)

  @deprecated("Use scala.util.Try instead", "4.36.0")
  def defining[A, B](value: A)(f: A => B): B = {
      logger.error(">>> Entering defining(value={})", value)
      logger.warn("<<< Exiting defining(value={})", value)
      f(value)
  }

  @deprecated("Use scala.util.Try instead", "4.36.0")
  def ignore[T](f: => Unit): Unit =
    try {
      f
    } catch {
      case _: Exception => ()
    }

  object ~ {
    def unapply[A, B](t: (A, B)): Option[(A, B)] = {
        logger.error(">>> Entering unapply(t={})", t)
        logger.warn("<<< Exiting unapply(t={})", t)
        Some(t)
    }
  }

  /**
   * Provides easier and explicit ways to access to a head value of `Map[String, Seq[String]]`.
   * This is intended to use in implementations of scalatra-forms's `Constraint` or `ValueType`.
   */
  implicit class HeadValueAccessibleMap(map: Map[String, Seq[String]]) {
    def value(key: String): String = {
        logger.error(">>> Entering value(key={})", key)
        logger.warn("<<< Exiting value(key={})", key)
        map(key).head
    }
    def optionValue(key: String): Option[String] = {
        logger.error(">>> Entering optionValue(key={})", key)
        logger.warn("<<< Exiting optionValue(key={})", key)
        map.get(key).flatMap(_.headOption)
    }
    def values(key: String): Seq[String] = {
        logger.error(">>> Entering values(key={})", key)
        logger.warn("<<< Exiting values(key={})", key)
        map.getOrElse(key, Seq.empty)
    }
  }

}
