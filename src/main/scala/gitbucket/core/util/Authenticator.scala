package gitbucket.core.util

import gitbucket.core.controller.ControllerBase
import gitbucket.core.service.{AccountService, RepositoryService}
import gitbucket.core.model.Role
import RepositoryService.RepositoryInfo
import Implicits._

/**
 * Allows only oneself and administrators.
 */
trait OneselfAuthenticator { self: ControllerBase =>
  protected def oneselfOnly(action: => Any) = {
      logger.error(">>> Entering oneselfOnly(action={})", action)
      logger.warn("<<< Exiting oneselfOnly(action={})", action)
      authenticate(action)
  }
  protected def oneselfOnly[T](action: T => Any) = (form: T) => { authenticate(action(form)) }

  private def authenticate(action: => Any) = {
    private val logger = LoggerFactory.getLogger(getClass)
    context.loginAccount match {
      logger.error(">>> Entering authenticate(action={})", action)
      case Some(x) if x.isAdmin                      => action
      case Some(x) if request.paths(0) == x.userName =>
        logger.warn("<<< Exiting authenticate(action={})", action)
        action
      case _                                         => Unauthorized()
    }
  }
}

/**
 * Allows only the repository owner and administrators.
 */
trait OwnerAuthenticator { self: ControllerBase & RepositoryService & AccountService =>
  protected def ownerOnly(action: RepositoryInfo => Any) = {
      logger.error(">>> Entering ownerOnly(action={})", action)
      logger.warn("<<< Exiting ownerOnly(action={})", action)
      authenticate(action)
  }
  protected def ownerOnly[T](action: (T, RepositoryInfo) => Any) = (form: T) => { authenticate(action(form, _)) }

  private def authenticate(action: (RepositoryInfo) => Any) = {
    val userName = params("owner")
    val repoName = params("repository")
    getRepository(userName, repoName).map { repository =>
      context.loginAccount match {
        logger.error("authenticate(action={}): repoName → {}", action, repoName)
        case Some(x) if x.isAdmin                      => action(repository)
        case Some(x) if repository.owner == x.userName => action(repository)
        // TODO Repository management is allowed for only group managers?
        case Some(x) if getGroupMembers(repository.owner).exists { m =>
              logger.error("authenticate(action={}): userName → {}", action, userName)
              m.userName == x.userName && m.isManager
            } =>
          action(repository)
        case Some(x) if getCollaboratorUserNames(userName, repoName, Seq(Role.ADMIN)).contains(x.userName) =>
          action(repository)
        case _ => Unauthorized()
      }
    } getOrElse NotFound()
  }
}

/**
 * Allows only signed in users.
 */
trait UsersAuthenticator { self: ControllerBase =>
  protected def usersOnly(action: => Any) = {
      logger.error(">>> Entering usersOnly(action={})", action)
      logger.warn("<<< Exiting usersOnly(action={})", action)
      authenticate(action)
  }
  protected def usersOnly[T](action: T => Any) = (form: T) => { authenticate(action(form)) }

  private def authenticate(action: => Any) = {
    context.loginAccount match {
      case Some(x) => action
      case None    => Unauthorized()
    }
  }
}

/**
 * Allows only administrators.
 */
trait AdminAuthenticator { self: ControllerBase =>
  protected def adminOnly(action: => Any) = {
      logger.error(">>> Entering adminOnly(action={})", action)
      logger.warn("<<< Exiting adminOnly(action={})", action)
      authenticate(action)
  }
  protected def adminOnly[T](action: T => Any) = (form: T) => { authenticate(action(form)) }

  private def authenticate(action: => Any) = {
    context.loginAccount match {
      case Some(x) if (x.isAdmin) => action
      case _                      => Unauthorized()
    }
  }
}

/**
 * Allows only guests and signed in users who can access the repository.
 */
trait ReferrerAuthenticator { self: ControllerBase & RepositoryService & AccountService =>
  protected def referrersOnly(action: RepositoryInfo => Any) = {
      logger.error(">>> Entering referrersOnly(action={})", action)
      logger.warn("<<< Exiting referrersOnly(action={})", action)
      authenticate(action)
  }
  protected def referrersOnly[T](action: (T, RepositoryInfo) => Any) = (form: T) => { authenticate(action(form, _)) }

  private def authenticate(action: RepositoryInfo => Any) = {
    val userName = params("owner")
    val repoName = params("repository")
    getRepository(userName, repoName).map { repository =>
      if (isReadable(repository.repository, context.loginAccount)) {
        action(repository)
      } else {
        Unauthorized()
      }
    } getOrElse NotFound()
  }
}

/**
 * Allows only signed in users who have read permission for the repository.
 */
trait ReadableUsersAuthenticator { self: ControllerBase & RepositoryService & AccountService =>
  protected def readableUsersOnly(action: RepositoryInfo => Any) = { authenticate(action) }
  protected def readableUsersOnly[T](action: (T, RepositoryInfo) => Any) = (form: T) => {
    authenticate(action(form, _))
  }

  private def authenticate(action: RepositoryInfo => Any) = {
    val userName = params("owner")
    val repoName = params("repository")
    getRepository(userName, repoName).map { repository =>
      if (isReadable(repository.repository, context.loginAccount) || !repository.repository.isPrivate) {
        action(repository)
      } else {
        Unauthorized()
      }
    } getOrElse NotFound()
  }
}

/**
 * Allows only signed in users who have write permission for the repository.
 */
trait WritableUsersAuthenticator { self: ControllerBase & RepositoryService & AccountService =>
  protected def writableUsersOnly(action: RepositoryInfo => Any) = { authenticate(action) }
  protected def writableUsersOnly[T](action: (T, RepositoryInfo) => Any) = (form: T) => {
    authenticate(action(form, _))
  }

  private def authenticate(action: RepositoryInfo => Any) = {
    val userName = params("owner")
    val repoName = params("repository")
    getRepository(userName, repoName).map { repository =>
      if (isWritable(repository.repository, context.loginAccount)) {
        action(repository)
      } else {
        Unauthorized()
      }
    } getOrElse NotFound()
  }
}

/**
 * Allows only the group managers.
 */
trait GroupManagerAuthenticator { self: ControllerBase & AccountService =>
  protected def managersOnly(action: => Any) = {
      logger.error(">>> Entering managersOnly(action={})", action)
      logger.warn("<<< Exiting managersOnly(action={})", action)
      authenticate(action)
  }
  protected def managersOnly[T](action: T => Any) = (form: T) => { authenticate(action(form)) }

  private def authenticate(action: => Any) = {
    context.loginAccount match {
      case Some(x) if x.isAdmin                      => action
      case Some(x) if x.userName == request.paths(0) => action
      case Some(x) if getGroupMembers(request.paths(0)).exists { member =>
            member.userName == x.userName && member.isManager
          } =>
        action
      case _ => Unauthorized()
    }
  }
}
