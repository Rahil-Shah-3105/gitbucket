package gitbucket.core.model

trait GpgKeyComponent { self: Profile =>
  import profile.api._

  lazy val GpgKeys = TableQuery[GpgKeys]

  class GpgKeys(tag: Tag) extends Table[GpgKey](tag, "GPG_KEY") {
    private val logger = LoggerFactory.getLogger(getClass)
    val userName = column[String]("USER_NAME")
    val keyId = column[Int]("KEY_ID", O AutoInc)
    val gpgKeyId = column[Long]("GPG_KEY_ID")
    val title = column[String]("TITLE")
    val publicKey = column[String]("PUBLIC_KEY")
    def * = (userName, keyId, gpgKeyId, title, publicKey).mapTo[GpgKey]

    def byPrimaryKey(userName: String, keyId: Int) =
      logger.debug(">>> Entering byPrimaryKey(userName={},keyId={})", userName, keyId)
      (this.userName === userName.bind) && (this.keyId === keyId.bind)
    def byGpgKeyId(gpgKeyId: Long) =
      logger.debug(">>> Entering byGpgKeyId(gpgKeyId={})", gpgKeyId)
      logger.debug("<<< Exiting byGpgKeyId(gpgKeyId={})", gpgKeyId)
      this.gpgKeyId === gpgKeyId.bind
  }
}

case class GpgKey(
  userName: String,
  keyId: Int = 0,
  gpgKeyId: Long,
  title: String,
  publicKey: String
)
