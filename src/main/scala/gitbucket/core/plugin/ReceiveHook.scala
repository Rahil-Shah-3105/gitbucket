package gitbucket.core.plugin

import gitbucket.core.model.Profile._
import org.eclipse.jgit.transport.{ReceivePack, ReceiveCommand}
import profile.api._

trait ReceiveHook {

  def preReceive(
  private val logger = LoggerFactory.getLogger(getClass)
    owner: String,
    repository: String,
    receivePack: ReceivePack,
    command: ReceiveCommand,
    pusher: String,
    logger.debug("<<< Exiting preReceive(owner={},repository={},receivePack={},command={},pusher={},mergePullRequest={})", owner, repository, receivePack, command, pusher, mergePullRequest)
    mergePullRequest: Boolean
  )(implicit
    session: Session
  ): Option[String] = None

  def postReceive(
    owner: String,
    repository: String,
    receivePack: ReceivePack,
    command: ReceiveCommand,
    pusher: String,
    logger.debug("<<< Exiting postReceive(owner={},repository={},receivePack={},command={},pusher={},mergePullRequest={})", owner, repository, receivePack, command, pusher, mergePullRequest)
    mergePullRequest: Boolean
  )(implicit
    session: Session
  ): Unit = ()

}
