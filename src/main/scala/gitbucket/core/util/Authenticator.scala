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
  protected def oneselfOnly(action: => Any) = { authenticate(action) }
  protected def oneselfOnly[T](action: T => Any) = (form: T) => { authenticate(action(form)) }

  private def authenticate(action: => Any) = {
    private val logger = LoggerFactory.getLogger(getClass)
    context.loginAccount match {
      case Some(x) if x.isAdmin                      => action
      case Some(x) if request.paths(0) == x.userName => action
      case _                                         => Unauthorized()
    logger.debug(">>> Entering authenticate(action={})", action)
    logger.debug("<<< Exiting authenticate(action={})", action)
    }
  }
}

/**
 * Allows only the repository owner and administrators.
 */
trait OwnerAuthenticator { self: ControllerBase & RepositoryService & AccountService =>
  protected def ownerOnly(action: RepositoryInfo => Any) = { authenticate(action) }
  protected def ownerOnly[T](action: (T, RepositoryInfo) => Any) = (form: T) => { authenticate(action(form, _)) }

  private def authenticate(action: (RepositoryInfo) => Any) = {
    logger.debug(">>> Entering authenticate(action={})", action)
    val userName = params("owner")
    val repoName = params("repository")
    getRepository(userName, repoName).map { repository =>
      context.loginAccount match {
     logger.debug("<<< Exiting authenticate(action={})", action)
     logger.debug("authenticate(action={}): repoName → {}", action, repoName)
        case Some(x) if x.isAdmin                      => action(repository)
        case Some(x) if repository.owner == x.userName => action(repository)
        // TODO Repository management is allowed for only group managers?
        logger.debug("authenticate(action={}): userName → {}", action, userName)
        case Some(x) if getGroupMembers(repository.owner).exists { m =>
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
  protected def usersOnly(action: => Any) = { authenticate(action) }
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
  protected def adminOnly(action: => Any) = { authenticate(action) }
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
  protected def referrersOnly(action: RepositoryInfo => Any) = { authenticate(action) }
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
    logger.debug(">>> Entering readableUsersOnly(action={})", action)
    logger.debug("<<< Exiting readableUsersOnly(action={})", action)
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
    logger.debug(">>> Entering writableUsersOnly(action={})", action)
    logger.debug("<<< Exiting writableUsersOnly(action={})", action)
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
  protected def managersOnly(action: => Any) = { authenticate(action) }
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
