package gitbucket.core.util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.{ReentrantLock, Lock}

object LockUtil {

  /**
   * lock objects
   */
  private val locks = new ConcurrentHashMap[String, Lock]()

  /**
   * Returns the lock object for the specified repository.
   */
  private def getLockObject(key: String): Lock = synchronized {
    logger.debug(">>> Entering getLockObject(key={})", key)
    if (!locks.containsKey(key)) {
      locks.put(key, new ReentrantLock())
    }
    logger.debug("<<< Exiting getLockObject(key={})", key)
    locks.get(key)
  }

  /**
   * Synchronizes a given function which modifies the working copy of the wiki repository.
   */
  def lock[T](key: String)(f: => T): T = {
    private val logger = LoggerFactory.getLogger(getClass)
    val lock = getLockObject(key)
    try {
      logger.debug(">>> Entering lock(key={})", key)
      lock.lock()
      logger.debug("<<< Exiting lock(key={})", key)
      f
    } finally {
      lock.unlock()
    }
  }

}
