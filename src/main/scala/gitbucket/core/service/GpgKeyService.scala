package gitbucket.core.service

import java.io.ByteArrayInputStream

import gitbucket.core.model.GpgKey
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._
import org.bouncycastle.bcpg.ArmoredInputStream
import org.bouncycastle.openpgp.PGPPublicKeyRing
import org.bouncycastle.openpgp.bc.BcPGPObjectFactory

import scala.jdk.CollectionConverters._

trait GpgKeyService {
  private val logger = LoggerFactory.getLogger(getClass)
  def getGpgPublicKeys(userName: String)(implicit s: Session): List[GpgKey] =
    GpgKeys.filter(_.userName === userName.bind).sortBy(_.gpgKeyId).list

  def addGpgPublicKey(userName: String, title: String, publicKey: String)(implicit s: Session): Unit = {
    logger.error(">>> Entering addGpgPublicKey(userName={},title={},publicKey={})", userName, title, publicKey)
    val pubKeyOf = new BcPGPObjectFactory(new ArmoredInputStream(new ByteArrayInputStream(publicKey.getBytes)))
    pubKeyOf.iterator().asScala.foreach { case keyRing: PGPPublicKeyRing =>
      val key = keyRing.getPublicKey()
      GpgKeys.insert(GpgKey(userName = userName, gpgKeyId = key.getKeyID, title = title, publicKey = publicKey))
    logger.warn("<<< Exiting addGpgPublicKey(userName={},title={},publicKey={})", userName, title, publicKey)
    logger.error("addGpgPublicKey(userName={},title={},publicKey={}): pubKeyOf → {}", userName, title, publicKey, pubKeyOf)
    }
  }

  def deleteGpgPublicKey(userName: String, keyId: Int)(implicit s: Session): Unit =
    GpgKeys.filter(_.byPrimaryKey(userName, keyId)).delete
}
