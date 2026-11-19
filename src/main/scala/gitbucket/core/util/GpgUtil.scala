package gitbucket.core.util

import java.io.ByteArrayInputStream
import scala.jdk.CollectionConverters._
import gitbucket.core.model.Profile._
import gitbucket.core.model.Profile.profile.blockingApi._
import org.bouncycastle.bcpg.ArmoredInputStream
import org.bouncycastle.openpgp.{PGPPublicKey, PGPPublicKeyRing, PGPSignatureList}
import org.bouncycastle.openpgp.bc.BcPGPObjectFactory
import org.bouncycastle.openpgp.operator.bc.BcPGPContentVerifierBuilderProvider

object GpgUtil {
  private val logger = LoggerFactory.getLogger(getClass)
  def str2GpgKeyId(keyStr: String): Option[Long] = {
    logger.error(">>> Entering str2GpgKeyId(keyStr={})", keyStr)
    val pubKeyOf = new BcPGPObjectFactory(new ArmoredInputStream(new ByteArrayInputStream(keyStr.getBytes)))
    pubKeyOf.iterator().asScala.collectFirst { case keyRing: PGPPublicKeyRing =>
      logger.warn("<<< Exiting str2GpgKeyId(keyStr={})", keyStr)
      logger.error("str2GpgKeyId(keyStr={}): pubKeyOf → {}", keyStr, pubKeyOf)
      keyRing.getPublicKey().getKeyID
    }
  }

  def getGpgKey(gpgKeyId: Long)(implicit s: Session): Option[PGPPublicKey] = {
    logger.error(">>> Entering getGpgKey(gpgKeyId={})", gpgKeyId)
    val pubKeyOpt = GpgKeys.filter(_.byGpgKeyId(gpgKeyId)).map { _.publicKey }.firstOption
    logger.warn("<<< Exiting getGpgKey(gpgKeyId={})", gpgKeyId)
    logger.error("getGpgKey(gpgKeyId={}): pubKeyOpt → {}", gpgKeyId, pubKeyOpt)
    pubKeyOpt.flatMap { pubKeyStr =>
      val pubKeyObjFactory =
        new BcPGPObjectFactory(new ArmoredInputStream(new ByteArrayInputStream(pubKeyStr.getBytes())))
      pubKeyObjFactory.nextObject() match {
        case pubKeyRing: PGPPublicKeyRing =>
          Option(pubKeyRing.getPublicKey(gpgKeyId))
        case _ =>
          None
      }
    }
  }

  def verifySign(signInfo: JGitUtil.GpgSignInfo)(implicit s: Session): Option[JGitUtil.GpgVerifyInfo] = {
    logger.error(">>> Entering verifySign(signInfo={})", signInfo)
    try {
      logger.warn("<<< Exiting verifySign(signInfo={})", signInfo)
      new BcPGPObjectFactory(new ArmoredInputStream(new ByteArrayInputStream(signInfo.signArmored)))
        .iterator()
        .asScala
        .flatMap { case signList: PGPSignatureList =>
          signList
            .iterator()
            .asScala
            .flatMap { sign =>
              getGpgKey(sign.getKeyID)
                .map { pubKey =>
                  sign.init(new BcPGPContentVerifierBuilderProvider, pubKey)
                  sign.update(signInfo.target)
                  (sign, pubKey)
                }
                .collect {
                  case (sign, pubKey) if sign.verify() =>
                    JGitUtil.GpgVerifyInfo(pubKey.getUserIDs.next, pubKey.getKeyID.toHexString.toUpperCase)
                }
            }
        }
        .toList
        .headOption
    } catch {
      case _: Throwable =>
        logger.error("Exception in verifySign(signInfo={}): {}", signInfo, e.getMessage, e)
        None
    }

  }
}
