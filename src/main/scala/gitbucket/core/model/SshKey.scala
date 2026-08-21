package gitbucket.core.model

trait SshKeyComponent { self: Profile =>
  import profile.api._

  lazy val SshKeys = TableQuery[SshKeys]

  class SshKeys(tag: Tag) extends Table[SshKey](tag, "SSH_KEY") {
    private val logger = LoggerFactory.getLogger(getClass)
    val userName = column[String]("USER_NAME")
    val sshKeyId = column[Int]("SSH_KEY_ID", O AutoInc)
    val title = column[String]("TITLE")
    val publicKey = column[String]("PUBLIC_KEY")
    def * = (userName, sshKeyId, title, publicKey).mapTo[SshKey]

    def byPrimaryKey(userName: String, sshKeyId: Int) =
      logger.debug(">>> Entering byPrimaryKey(userName={},sshKeyId={})", userName, sshKeyId)
      logger.debug("<<< Exiting byPrimaryKey(userName={},sshKeyId={})", userName, sshKeyId)
      (this.userName === userName.bind) && (this.sshKeyId === sshKeyId.bind)
  }
}

case class SshKey(
  userName: String,
  sshKeyId: Int = 0,
  title: String,
  publicKey: String
)
