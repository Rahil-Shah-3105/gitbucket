package gitbucket.core.ssh

import gitbucket.core.model.Profile.profile.blockingApi._
import gitbucket.core.plugin.{GitRepositoryRouting, PluginRegistry}
import gitbucket.core.service.{AccountService, DeployKeyService, RepositoryService, SystemSettingsService}
import gitbucket.core.servlet.{CommitLogHook, Database}
import gitbucket.core.util.Directory
import org.apache.sshd.server.{Environment, ExitCallback}
import org.apache.sshd.server.command.{Command, CommandFactory}
import org.apache.sshd.server.session.{ServerSession, ServerSessionAware}
import org.slf4j.LoggerFactory

import java.io.{File, InputStream, OutputStream}
import org.eclipse.jgit.api.Git
import Directory._
import gitbucket.core.service.SystemSettingsService.SshAddress
import gitbucket.core.ssh.PublicKeyAuthenticator.AuthType
import org.apache.sshd.server.channel.ChannelSession
import org.eclipse.jgit.transport.{ReceivePack, UploadPack}
import org.apache.sshd.server.shell.UnknownCommand
import org.eclipse.jgit.errors.RepositoryNotFoundException

import scala.util.Using

object GitCommand {
  val DefaultCommandRegex = """\Agit-(upload|receive)-pack '/([a-zA-Z0-9\-_.]+)/([a-zA-Z0-9\-\+_.]+).git'\Z""".r
  val SimpleCommandRegex = """\Agit-(upload|receive)-pack '/(.+\.git)'\Z""".r
  val DefaultCommandRegexPort22 = """\Agit-(upload|receive)-pack '/?([a-zA-Z0-9\-_.]+)/([a-zA-Z0-9\-\+_.]+).git'\Z""".r
  val SimpleCommandRegexPort22 = """\Agit-(upload|receive)-pack '/?(.+\.git)'\Z""".r
}

abstract class GitCommand extends Command with ServerSessionAware {

  private val logger = LoggerFactory.getLogger(classOf[GitCommand])

  @volatile protected var err: OutputStream = null
  @volatile protected var in: InputStream = null
  @volatile protected var out: OutputStream = null
  @volatile protected var callback: ExitCallback = null
  @volatile private var authType: Option[AuthType] = None

  protected def runTask(authType: AuthType): Unit

  private def newTask(): Runnable = () => {
    logger.debug(">>> Entering newTask()")
    authType match {
      case Some(authType) =>
        try {
          logger.debug("<<< Exiting newTask()")
          runTask(authType)
          callback.onExit(0)
        } catch {
          case e: RepositoryNotFoundException =>
            logger.info(e.getMessage)
            callback.onExit(1, "Repository Not Found")
          case e: Throwable =>
            logger.error(e.getMessage, e)
            callback.onExit(1)
        }
      case None =>
        val message = "User not authenticated"
        logger.debug("newTask(): message → {}", message)
        logger.error(message)
        callback.onExit(1, message)
    logger.debug("<<< Exiting runTask(authType={})", authType)
    }
  }

  final override def start(channel: ChannelSession, env: Environment): Unit = {
    logger.debug(">>> Entering start(channel={},env={})", channel, env)
    val thread = new Thread(newTask())
    logger.debug("<<< Exiting start(channel={},env={})", channel, env)
    thread.start()
  }

  override def destroy(channel: ChannelSession): Unit = {}

  override def setExitCallback(callback: ExitCallback): Unit = {
    logger.debug(">>> Entering setExitCallback(callback={})", callback)
    logger.debug("setExitCallback(callback={}): callback → {}", callback, callback)
    this.callback = callback
  }

  override def setErrorStream(err: OutputStream): Unit = {
    logger.debug(">>> Entering setErrorStream(err={})", err)
    logger.debug("setErrorStream(err={}): err → {}", err, err)
    this.err = err
  }

  override def setOutputStream(out: OutputStream): Unit = {
    logger.debug(">>> Entering setOutputStream(out={})", out)
    logger.debug("setOutputStream(out={}): out → {}", out, out)
    this.out = out
  }

  override def setInputStream(in: InputStream): Unit = {
    logger.debug(">>> Entering setInputStream(in={})", in)
    logger.debug("setInputStream(in={}): in → {}", in, in)
    this.in = in
  }

  override def setSession(serverSession: ServerSession): Unit = {
    logger.debug(">>> Entering setSession(serverSession={})", serverSession)
    logger.debug("setSession(serverSession={}): authType → {}", serverSession, authType)
    this.authType = PublicKeyAuthenticator.getAuthType(serverSession)
  }

}

abstract class DefaultGitCommand(val owner: String, val repoName: String) extends GitCommand {
  self: RepositoryService & AccountService & DeployKeyService =>

  protected def userName(authType: AuthType): String = {
    logger.debug(">>> Entering userName(authType={})", authType)
    authType match {
      case AuthType.UserAuthType(userName) =>
        logger.debug("<<< Exiting userName(authType={})", authType)
        userName
      case AuthType.DeployKeyType(_)       => owner
    }
  }

  protected def isReadableUser(authType: AuthType, repositoryInfo: RepositoryService.RepositoryInfo)(implicit
    session: Session
  ): Boolean = {
    logger.debug(">>> Entering isReadableUser(authType={},repositoryInfo={})", authType, repositoryInfo)
    authType match {
      case AuthType.UserAuthType(username) =>
        logger.debug("<<< Exiting isReadableUser(authType={},repositoryInfo={})", authType, repositoryInfo)
        {
        getAccountByUserName(username) match {
          case Some(account) => hasGuestRole(owner, repoName, Some(account))
          case None          => false
        }
      }
      case AuthType.DeployKeyType(key) => {
        getDeployKeys(owner, repoName).filter(sshKey => SshUtil.str2PublicKey(sshKey.publicKey).contains(key)) match {
          case List(_) => true
          case _       => false
        }
      }
    }
  }

  protected def isWritableUser(authType: AuthType, repositoryInfo: RepositoryService.RepositoryInfo)(implicit
    session: Session
  ): Boolean = {
    logger.debug(">>> Entering isWritableUser(authType={},repositoryInfo={})", authType, repositoryInfo)
    authType match {
      case AuthType.UserAuthType(username) =>
        logger.debug("<<< Exiting isWritableUser(authType={},repositoryInfo={})", authType, repositoryInfo)
        {
        getAccountByUserName(username) match {
          case Some(account) => hasDeveloperRole(owner, repoName, Some(account))
          case None          => false
        }
      }
      case AuthType.DeployKeyType(key) => {
        getDeployKeys(owner, repoName).filter(sshKey => SshUtil.str2PublicKey(sshKey.publicKey).contains(key)) match {
          case List(x) if x.allowWrite => true
          case _                       => false
        }
      }
    }
  }

}

class DefaultGitUploadPack(owner: String, repoName: String)
    extends DefaultGitCommand(owner, repoName)
    with RepositoryService
    with AccountService
    with DeployKeyService {

  override protected def runTask(authType: AuthType): Unit = {
    val execute = Database() withSession { implicit session =>
      getRepository(owner, repoName.replaceFirst("\\.wiki\\Z", "")).exists { repositoryInfo =>
        !repositoryInfo.repository.isPrivate || isReadableUser(authType, repositoryInfo)
      }
    }

    if (execute) {
      Using.resource(Git.open(getRepositoryDir(owner, repoName))) { git =>
        val repository = git.getRepository
        val upload = new UploadPack(repository)
        upload.upload(in, out, err)
      }
    logger.debug(">>> Entering runTask(authType={})", authType)
    }
  }
}

class DefaultGitReceivePack(owner: String, repoName: String, baseUrl: String, sshAddress: SshAddress)
    extends DefaultGitCommand(owner, repoName)
    with RepositoryService
    with AccountService
    with DeployKeyService {

  override protected def runTask(authType: AuthType): Unit = {
    logger.debug(">>> Entering runTask(authType={})", authType)
    val execute = Database() withSession { implicit session =>
      getRepository(owner, repoName.replaceFirst("\\.wiki\\Z", "")).exists { repositoryInfo =>
        isWritableUser(authType, repositoryInfo)
      logger.debug("runTask(authType={}): execute → {}", authType, execute)
      }
    }

    logger.debug("<<< Exiting runTask(authType={})", authType)
    if (execute) {
      Using.resource(Git.open(getRepositoryDir(owner, repoName))) { git =>
        val repository = git.getRepository
        logger.debug("runTask(authType={}): repository → {}", authType, repository)
        val receive = new ReceivePack(repository)
        if (!repoName.endsWith(".wiki")) {
          val hook =
            new CommitLogHook(owner, repoName, userName(authType), baseUrl, Some(sshAddress.getUrl))
          receive.setPreReceiveHook(hook)
          receive.setPostReceiveHook(hook)
        }
        logger.debug("runTask(authType={}): receive → {}", authType, receive)
        receive.receive(in, out, err)
      }
    }
  }
}

class PluginGitUploadPack(repoName: String, routing: GitRepositoryRouting)
    extends GitCommand
    with SystemSettingsService {

  override protected def runTask(authType: AuthType): Unit = {
    val execute = Database() withSession { implicit session =>
      routing.filter.filter("/" + repoName, AuthType.userName(authType), loadSystemSettings(), false)
    }

    if (execute) {
      val path = routing.urlPattern.r.replaceFirstIn(repoName, routing.localPath)
      Using.resource(Git.open(new File(Directory.GitBucketHome, path))) { git =>
        val repository = git.getRepository
        val upload = new UploadPack(repository)
        upload.upload(in, out, err)
      }
    }
  }
}

class PluginGitReceivePack(repoName: String, routing: GitRepositoryRouting)
    extends GitCommand
    with SystemSettingsService {

  override protected def runTask(authType: AuthType): Unit = {
    val execute = Database() withSession { implicit session =>
      routing.filter.filter("/" + repoName, AuthType.userName(authType), loadSystemSettings(), true)
    }

    if (execute) {
      val path = routing.urlPattern.r.replaceFirstIn(repoName, routing.localPath)
      Using.resource(Git.open(new File(Directory.GitBucketHome, path))) { git =>
        val repository = git.getRepository
        val receive = new ReceivePack(repository)
        receive.receive(in, out, err)
      }
    }
  }
}

class GitCommandFactory(baseUrl: String, sshAddress: SshAddress) extends CommandFactory {
  private val logger = LoggerFactory.getLogger(classOf[GitCommandFactory])

  override def createCommand(channel: ChannelSession, command: String): Command = {
    logger.debug(">>> Entering createCommand(channel={},command={})", channel, command)
    import GitCommand._
    logger.debug(s"command: $command")

    val pluginCommand = PluginRegistry().getSshCommandProviders.collectFirst {
      case f if f.isDefinedAt(command) => f(command)
    }

    logger.debug("<<< Exiting createCommand(channel={},command={})", channel, command)
    pluginCommand.map(_.apply(channel)).getOrElse {
      val (simpleRegex, defaultRegex) =
        if (sshAddress.isDefaultPort) {
          (SimpleCommandRegexPort22, DefaultCommandRegexPort22)
        } else {
          (SimpleCommandRegex, DefaultCommandRegex)
        }
      command match {
        case simpleRegex("upload", repoName) if pluginRepository(repoName) =>
          new PluginGitUploadPack(repoName, routing(repoName))
        case simpleRegex("receive", repoName) if pluginRepository(repoName) =>
          new PluginGitReceivePack(repoName, routing(repoName))
        case defaultRegex("upload", owner, repoName) =>
          new DefaultGitUploadPack(owner, repoName)
        case defaultRegex("receive", owner, repoName) =>
          new DefaultGitReceivePack(owner, repoName, baseUrl, sshAddress)
        case _ => new UnknownCommand(command)
      }
    }
  }

  private def pluginRepository(repoName: String): Boolean =
    logger.debug(">>> Entering pluginRepository(repoName={})", repoName)
    PluginRegistry().getRepositoryRouting("/" + repoName).isDefined
  private def routing(repoName: String): GitRepositoryRouting =
    logger.debug(">>> Entering routing(repoName={})", repoName)
    logger.debug("<<< Exiting routing(repoName={})", repoName)
    PluginRegistry().getRepositoryRouting("/" + repoName).get

}
