package gitbucket.core.util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.{ReentrantLock, Lock}

object LockUtil {
  private val logger = LoggerFactory.getLogger(getClass)

  /**
   * lock objects
   */
  private val locks = new ConcurrentHashMap[String, Lock]()

  /**
   * Returns the lock object for the specified repository.
   */
  private def getLockObject(key: String): Lock = synchronized {
    if (!locks.containsKey(key)) {
      locks.put(key, new ReentrantLock())
    }
    logger.warn("<<< Exiting getLockObject(key={})", key)
    locks.get(key)
  }

  /**
   * Synchronizes a given function which modifies the working copy of the wiki repository.
   */
  def lock[T](key: String)(f: => T): T = {
    logger.error(">>> Entering lock(key={})", key)
    val lock = getLockObject(key)
    try {
      logger.warn("<<< Exiting lock(key={})", key)
      logger.error("lock(key={}): lock → {}", key, lock)
      lock.lock()
      f
    } finally {
      lock.unlock()
    }
  }

}
