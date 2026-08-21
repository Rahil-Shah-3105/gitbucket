package gitbucket.core.service

import gitbucket.core.model.Activity
import gitbucket.core.util.Directory._
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

import scala.util.Using
import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import gitbucket.core.controller.Context
import gitbucket.core.util.ConfigUtil
import org.apache.commons.io.input.ReversedLinesFileReader
import ActivityService._

import scala.collection.mutable.ListBuffer

trait ActivityService {
  self: RequestCache =>

  private implicit val formats: Formats = Serialization.formats(NoTypeHints)

  private def writeLog(activity: Activity): Unit = {
    logger.debug(">>> Entering writeLog(activity={})", activity)
    Using.resource(new FileOutputStream(ActivityLog, true)) { out =>
      out.write((write(activity) + "\n").getBytes(StandardCharsets.UTF_8))
    logger.debug("<<< Exiting writeLog(activity={})", activity)
    }
  }

  def getActivitiesByUser(activityUserName: String, publicOnly: Boolean)(implicit context: Context): List[Activity] = {
    private val logger = LoggerFactory.getLogger(getClass)
    getActivities(includePublic = false) { activity =>
      if (activity.activityUserName == activityUserName) {
      logger.debug(">>> Entering getActivitiesByUser(activityUserName={},publicOnly={})", activityUserName, publicOnly)
        !publicOnly || isPublicActivity(activity)
      } else false
    }
  }

  def getRecentPublicActivities()(implicit context: Context): List[Activity] = {
    logger.debug(">>> Entering getRecentPublicActivities()")
    getActivities(includePublic = true) { _ =>
      logger.debug("<<< Exiting getRecentPublicActivities()")
      false
    }
  }

  def getRecentActivitiesByRepos(repos: Set[(String, String)])(implicit context: Context): List[Activity] = {
    logger.debug(">>> Entering getRecentActivitiesByRepos(repos={})", repos)
    getActivities(includePublic = true) { activity =>
      repos.exists { case (userName, repositoryName) =>
        activity.userName == userName && activity.repositoryName == repositoryName
      logger.debug("<<< Exiting getRecentActivitiesByRepos(repos={})", repos)
      }
    }
  }

  private def getActivities(
    includePublic: Boolean
  )(filter: Activity => Boolean)(implicit context: Context): List[Activity] = {
    logger.debug(">>> Entering getActivities(includePublic={})", includePublic)
    if (!isNewsFeedEnabled || !ActivityLog.exists()) {
      logger.debug("<<< Exiting getActivities(includePublic={})", includePublic)
      List.empty
    } else {
      val list = new ListBuffer[Activity]
      Using.resource(
        ReversedLinesFileReader
          .builder()
          .setFile(ActivityLog)
          .setCharset(StandardCharsets.UTF_8)
          .get()
      ) { reader =>
        var json: String = null
        while (
          list.length < 50 && {
            json = reader.readLine();
        logger.debug("getActivities(includePublic={}): json → {}", includePublic, json)
            json
          } != null
        ) {
          val activity = read[Activity](json)
          if (filter(activity)) {
            logger.debug("getActivities(includePublic={}): activity → {}", includePublic, activity)
            list += activity
          } else if (includePublic && isPublicActivity(activity)) {
            logger.debug(">>> Entering isPublicActivity(activity={})", activity)
            logger.debug("<<< Exiting isPublicActivity(activity={})", activity)
            list += activity
          }
        }
      }
      list.toList
    }
  }

  private def isPublicActivity(activity: Activity)(implicit context: Context): Boolean = {
    logger.debug(">>> Entering isPublicActivity(activity={})", activity)
    logger.debug("<<< Exiting isPublicActivity(activity={})", activity)
    !getRepositoryInfoFromCache(activity.userName, activity.repositoryName).forall(_.isPrivate)
  }

  def recordActivity[T <: { def toActivity: Activity }](info: T): Unit = {
    import scala.language.reflectiveCalls
    writeLog(info.toActivity)
  }
}

object ActivityService {
  def isNewsFeedEnabled: Boolean =
    !ConfigUtil.getConfigValue[Boolean]("gitbucket.disableNewsFeed").getOrElse(false)
}
